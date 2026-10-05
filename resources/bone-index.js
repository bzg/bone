// bone-index.js -- Client-side filtering, sorting, URL state, lazy-load closed.
// Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
// SPDX-License-Identifier: MPL-2.0
//
// Expects a global object:
//   boneConfig -- .typeLabels, .openJsonUrl, .closedJsonUrl,
//                 .pageSize, .sourceType
// Open reports are fetched from openJsonUrl at startup (see init at the
// bottom); .total/.openCount/.closedCount are filled in from that
// payload. Closed reports stay lazy-loaded from closedJsonUrl.

var allTypes = [];          // populated from the fetched open reports
var activeTypes = {};
var onlyOpen    = true;
var onlyAcked   = false;
var onlyOwned   = false;
var onlyAwaiting = false;

var closedLoaded = false;
var closedLoading = false;

var pageSize    = boneConfig.pageSize !== undefined ? boneConfig.pageSize : 50;
var currentPage = 1;

// Pre-compute today's timestamp for due-date calculations
var _today = new Date();
_today.setHours(0,0,0,0);
var _todayMs = _today.getTime();
var MS_PER_DAY = 86400000;

// Sort sentinels and rendering thresholds
var NO_DUE_DATE_SORT_VALUE = 99999; // rows without a deadline sort to the end
var SUBJECT_TRUNCATE_MIN_CHARS = 75; // below this length we never show the unfold toggle

// ── Data model ──────────────────────────────────────────────
// All data lives in JS arrays; DOM is only used for rendering the current page.
var _allReports = [];       // prepared report objects
var _filteredReports = [];  // filtered + sorted subset
var _displayList = [];      // after series and cluster folding (what pagination sees)
var _seriesFoldState = {};  // series-id -> true (folded) | false (unfolded); null = auto
var _clusterFoldState = {}; // cluster-id -> true (folded) | false (unfolded); null = auto
var _clusters = {};         // cluster-id -> { cid, mids, members, types, rep }
var _clusterOf = {};        // mid -> cluster-id
var _byMid = {};            // mid -> prepared report

function getSearchInput() { return document.getElementById('si'); }

function setSearch(val) {
  getSearchInput().value = val;
  currentPage = 1;
  filterReports();
  pushURL();
}

function showRelated(val) {
  // Reset types/acked/owned/awaiting; leave the Open button untouched.
  // Appending 'closed:true' to the query bypasses the Open filter
  // transiently -- clearing the search restores the previous state.
  onlyAcked = false;
  onlyOwned = false;
  onlyAwaiting = false;
  allTypes.forEach(function(t) { activeTypes[t] = true; });
  document.getElementById('btn-acked').classList.add('outline');
  document.getElementById('btn-owned').classList.add('outline');
  document.getElementById('btn-awaiting').classList.add('outline');
  document.querySelectorAll('.filters button[data-type]').forEach(function(btn) {
    btn.classList.remove('outline');
  });
  setSearch(val + ' closed:true');
}

// The query language itself (tokenizer, per-token predicate
// compilation, searchFields) lives in bone-search.js, inlined by the
// HTML shell right before this file and unit-tested under node.

/* ── Report preparation ──────────────────────────────────────── */

function abbreviateName(name) {
  var parts = name.trim().split(/\s+/);
  if (parts.length < 2) return name;
  return parts[0] + ' ' + parts[1].substring(0, 2) + '.';
}

function emailLocalPart(addr) {
  var at = addr.indexOf('@');
  return at > 0 ? addr.substring(0, at) : addr;
}

function escHtml(s) {
  return String(s).replace(/&/g,'&amp;').replace(/</g,'&lt;').replace(/>/g,'&gt;');
}
function escAttr(s) {
  return String(s).replace(/&/g,'&amp;').replace(/"/g,'&quot;').replace(/</g,'&lt;').replace(/>/g,'&gt;');
}

var _typeLabels = boneConfig.typeLabels || {};

function prepareReport(r) {
  var from = r.from || '';
  var fromName = r['from-name'] || '';
  var flags = r.flags || '---';
  var acked = r.acked || '';
  var owned = r.owned || '';
  var ownedName = r['owned-name'] || '';
  var deadline = r.deadline || '';

  var closedBool = flags.length >= 3 && flags[2] !== '-';
  var author = fromName ? abbreviateName(fromName) : emailLocalPart(from);
  var ownerDisplay = ownedName ? abbreviateName(ownedName)
                   : owned     ? emailLocalPart(owned)
                   : '';
  var flagsScore = (acked ? 1 : 0) + (owned ? 2 : 0) + (closedBool ? 0 : 4);

  var dueDays = null;
  if (deadline) {
    var parts = deadline.split('-');
    if (parts.length === 3) {
      var deadlineMs = new Date(+parts[0], +parts[1]-1, +parts[2]).getTime();
      dueDays = Math.round((deadlineMs - _todayMs) / MS_PER_DAY);
    }
  }

  // The searchFields (bone-search.js) carry everything the compiled
  // query predicates read; the rest is render/sort state.
  return Object.assign(searchFields(r), {
    raw: r,
    seriesId: (r.series && r.series.id) ? r.series.id : '',
    closed: closedBool,
    awaiting: !!(r.awaiting || false),
    lastActivity: r['last-activity'] || '',
    // Render helpers (pre-computed once)
    isoDate: parseIsoDate(r['date-raw'] || r.date || ''),
    author: author,
    ownerDisplay: ownerDisplay,
    flagsScore: flagsScore,
    dueDays: dueDays,
    replies: r.replies || 0
  });
}

/* ── Matching (operates on prepared report objects, not DOM) ── */

// The toolbar (type buttons, Open/Acked/Owned/Awaiting) pre-filters;
// the compiled clauses from bone-search.js do the query matching.
// An empty query has no clause and matches everything the toolbar lets
// through; a clause marked includeClosed bypasses the Open button.
function matchReport(rpt, clauses) {
  if (!activeTypes[rpt.type]) return false;
  if (onlyAcked  && rpt.acked === '') return false;
  if (onlyOwned  && rpt.owned === '') return false;
  if (onlyAwaiting && !rpt.awaiting) return false;
  if (clauses.length === 0) return !(onlyOpen && rpt.closed);
  return clauses.some(function(c) {
    if (onlyOpen && !c.includeClosed && rpt.closed) return false;
    return clauseMatches(rpt, c);
  });
}

function queryIncludesClosed(q) {
  if (!q) return false;
  return clausesIncludeClosed(compileQuery(q));
}

/* ── Filtering & Sorting (in-memory, no DOM access) ──────────── */

function filterReports() {
  var raw = getSearchInput().value;
  var clauses = compileQuery(raw);
  if (!closedLoaded && clausesIncludeClosed(clauses)) {
    loadClosedReports(function() { filterReports(); });
    return;
  }
  console.time('bone:filter');
  _filteredReports = [];
  for (var i = 0; i < _allReports.length; i++) {
    if (matchReport(_allReports[i], clauses)) {
      _filteredReports.push(_allReports[i]);
    }
  }
  // Re-apply current sort if active
  var sortKeys = Object.keys(sortState);
  if (sortKeys.length > 0) {
    sortReports(sortKeys[0], sortState[sortKeys[0]]);
  }
  buildDisplayList();
  console.timeEnd('bone:filter');
  console.time('bone:render');
  renderPage();
  console.timeEnd('bone:render');
}

function getSortValue(rpt, key) {
  switch(key) {
    case 'type':     return _typeLabels[rpt.type] || rpt.type;
    case 'priority': return rpt.priority;
    case 'due':      return rpt.dueDays !== null ? rpt.dueDays : NO_DUE_DATE_SORT_VALUE;
    case 'flags':    return rpt.flagsScore;
    case 'subject':  return rpt.lastActivity || '';
    case 'from':     return rpt.from;
    case 'owner':    return rpt.owned;
    case 'date':     return rpt.date;
    case 'replies':  return rpt.replies;
    default:         return '';
  }
}

function sortReports(key, dir) {
  _filteredReports.sort(function(a, b) {
    var av = getSortValue(a, key);
    var bv = getSortValue(b, key);
    if (typeof av === 'number' && typeof bv === 'number') {
      return dir === 'asc' ? av - bv : bv - av;
    }
    av = String(av); bv = String(bv);
    if (av === '' && bv !== '') return 1;
    if (av !== '' && bv === '') return -1;
    return dir === 'asc' ? av.localeCompare(bv) : bv.localeCompare(av);
  });
}

/* ── Series & cluster folding ───────────────────────────────── */

function flagsCat(flags) {
  var f = flags || '---';
  if (f.length >= 3 && f[2] !== '-') return 'C';
  if (f[0] !== '-') return 'A';
  return 'O';
}

function isSeriesHomogeneous(members) {
  var refCat = null;
  for (var i = 0; i < members.length; i++) {
    var seq = members[i].raw['patch-seq'] || '';
    if (seq.indexOf('0/') === 0) continue; // skip cover letter
    var cat = flagsCat(members[i].raw.flags);
    if (refCat === null) { refCat = cat; continue; }
    if (cat !== refCat) return false;
  }
  return true;
}

// Build clusters of reports connected by :related-to relations.
// Excludes series patches (they fold via the series mechanism).
// Singleton groups are dropped. Representative = oldest by date.
function computeClusters() {
  // Snapshot the previous mid->cid mapping so we can carry user fold state
  // across recomputes (e.g., after lazy-loading closed reports). The cid is
  // mids[0] (sorted), so a new member with a smaller mid would shift the cid
  // even though the cluster is "the same" from the user's perspective.
  var oldMidsByCid = {};
  for (var oldMid in _clusterOf) {
    if (!Object.prototype.hasOwnProperty.call(_clusterOf, oldMid)) continue;
    var oc = _clusterOf[oldMid];
    (oldMidsByCid[oc] = oldMidsByCid[oc] || []).push(oldMid);
  }

  _clusters = {};
  _clusterOf = {};
  _byMid = {};

  for (var i = 0; i < _allReports.length; i++) {
    var rpt = _allReports[i];
    if (rpt.mid) _byMid[rpt.mid] = rpt;
  }

  var parent = {};
  function find(x) {
    while (parent[x] !== x) {
      parent[x] = parent[parent[x]]; // half-path compression
      x = parent[x];
    }
    return x;
  }
  function union(a, b) {
    var ra = find(a), rb = find(b);
    if (ra !== rb) parent[ra] = rb;
  }

  for (var i = 0; i < _allReports.length; i++) {
    var rpt = _allReports[i];
    if (!rpt.mid || rpt.seriesId) continue;
    parent[rpt.mid] = rpt.mid;
  }

  for (var i = 0; i < _allReports.length; i++) {
    var rpt = _allReports[i];
    if (!rpt.mid || rpt.seriesId) continue;
    if (!(rpt.mid in parent)) continue;
    var related = rpt.raw['related-to'];
    if (!related) continue;
    for (var k = 0; k < related.length; k++) {
      var other = related[k];
      var otherMid = other && other['message-id'];
      if (!otherMid) continue;
      otherMid = String(otherMid).toLowerCase();
      if (otherMid in parent) union(rpt.mid, otherMid);
    }
  }

  var groups = {};
  for (var mid in parent) {
    if (!Object.prototype.hasOwnProperty.call(parent, mid)) continue;
    var root = find(mid);
    if (!groups[root]) groups[root] = [];
    groups[root].push(mid);
  }

  for (var root in groups) {
    if (!Object.prototype.hasOwnProperty.call(groups, root)) continue;
    var mids = groups[root];
    if (mids.length < 2) continue;
    mids.sort();
    var cid = mids[0];
    var members = [];
    var types = {};
    for (var j = 0; j < mids.length; j++) {
      var r = _byMid[mids[j]];
      if (r) {
        members.push(r);
        types[r.type] = 1;
        _clusterOf[mids[j]] = cid;
      }
    }
    if (members.length < 2) continue;
    members.sort(compareByDateThenMid);
    _clusters[cid] = {
      cid: cid,
      mids: mids,
      members: members,
      types: types,
      rep: members[0].mid
    };
  }

  // Carry over user fold state: for each old cid with state, find any of its
  // old members and look up its new cid. If the cluster split or merged, the
  // state lands on whichever new cluster won that member -- still the user's
  // last intent.
  var oldState = _clusterFoldState;
  _clusterFoldState = {};
  for (var oldCid in oldState) {
    if (!Object.prototype.hasOwnProperty.call(oldState, oldCid)) continue;
    var oldMembers = oldMidsByCid[oldCid];
    if (!oldMembers) continue;
    for (var im = 0; im < oldMembers.length; im++) {
      var newCid = _clusterOf[oldMembers[im]];
      if (newCid) {
        _clusterFoldState[newCid] = oldState[oldCid];
        break;
      }
    }
  }
}

function compareByDateThenMid(a, b) {
  var da = a.date || '', db = b.date || '';
  if (da < db) return -1;
  if (da > db) return 1;
  if (a.mid < b.mid) return -1;
  if (a.mid > b.mid) return 1;
  return 0;
}

function isAllTypesActive() {
  for (var i = 0; i < allTypes.length; i++) {
    if (!activeTypes[allTypes[i]]) return false;
  }
  return true;
}

function clusterFoldable(cluster) {
  if (isAllTypesActive()) return true;
  for (var t in cluster.types) {
    if (!Object.prototype.hasOwnProperty.call(cluster.types, t)) continue;
    if (!activeTypes[t]) return false;
  }
  return true;
}

function clusterStatusSummary(members) {
  var counts = {A: 0, C: 0, O: 0};
  for (var i = 0; i < members.length; i++) {
    counts[flagsCat(members[i].raw.flags)]++;
  }
  var parts = [];
  if (counts.A) parts.push(counts.A + ' acked');
  if (counts.C) parts.push(counts.C + ' closed');
  if (counts.O) parts.push(counts.O + ' open');
  return parts.join(', ');
}

function buildDisplayList() {
  var seriesGroups = {};
  var clusterGroups = {};
  var order = [];
  var result = [];

  for (var i = 0; i < _filteredReports.length; i++) {
    var rpt = _filteredReports[i];
    var sid = rpt.seriesId;
    var cid = sid ? null : _clusterOf[rpt.mid];
    var cluster = cid ? _clusters[cid] : null;
    // A cluster only groups its members when foldable (no type filter active,
    // or cluster types fit inside the filter). Otherwise members fall back
    // to standalone rows -- this is how mixed clusters dissolve under a type
    // filter, per the folding spec.
    if (sid) {
      if (!seriesGroups[sid]) {
        seriesGroups[sid] = [];
        order.push({kind: 'series', sid: sid, insertAt: result.length});
        result.push(null);
      }
      seriesGroups[sid].push(rpt);
    } else if (cluster && clusterFoldable(cluster)) {
      if (!clusterGroups[cid]) {
        clusterGroups[cid] = [];
        order.push({kind: 'cluster', cid: cid, insertAt: result.length});
        result.push(null);
      }
      clusterGroups[cid].push(rpt);
    } else {
      result.push(rpt);
    }
  }

  for (var j = 0; j < order.length; j++) {
    var entry = order[j];
    var insert;

    if (entry.kind === 'series') {
      var sid = entry.sid;
      var members = seriesGroups[sid];
      members.sort(function(a, b) {
        var sa = a.raw['patch-seq'] || '0/0', sb = b.raw['patch-seq'] || '0/0';
        var na = parseInt(sa, 10) || 0, nb = parseInt(sb, 10) || 0;
        return na - nb;
      });
      var manualState = _seriesFoldState[sid];
      var folded;
      if (manualState === true || manualState === false) folded = manualState;
      else folded = isSeriesHomogeneous(members);
      var rep = members[0];
      for (var k = 0; k < members.length; k++) {
        var seq = members[k].raw['patch-seq'] || '';
        if (seq.indexOf('0/') === 0) { rep = members[k]; break; }
      }
      var repEntry = Object.create(rep);
      repEntry._isSeries = true;
      repEntry._seriesFolded = folded;
      repEntry._seriesId = sid;
      repEntry._seriesMembers = members;
      insert = [repEntry];
      if (!folded) {
        for (var k = 0; k < members.length; k++) {
          if (members[k] !== rep) {
            var child = Object.create(members[k]);
            child._isSeriesChild = true;
            child._seriesId = sid;
            insert.push(child);
          }
        }
      }
    } else {
      var cid = entry.cid;
      var visible = clusterGroups[cid];
      var cluster = _clusters[cid];
      var repMid = cluster ? cluster.rep : null;
      var rep = null;
      for (var k = 0; k < visible.length; k++) {
        if (visible[k].mid === repMid) { rep = visible[k]; break; }
      }
      // Cluster reached this branch only if it was foldable at grouping time.
      // If the representative didn't survive the filter, hide the cluster.
      if (!rep || !cluster) {
        insert = [];
      } else {
        var manualState = _clusterFoldState[cid];
        var folded = (manualState === true || manualState === false)
          ? manualState
          : true; // default folded
        // Sort visible members oldest first (rep stays at index 0).
        visible.sort(compareByDateThenMid);
        var repEntry = Object.create(rep);
        repEntry._isCluster = true;
        repEntry._clusterFolded = folded;
        repEntry._clusterId = cid;
        repEntry._clusterMembers = visible;
        repEntry._clusterSummary = clusterStatusSummary(cluster.members);
        insert = [repEntry];
        if (!folded) {
          for (var k = 0; k < visible.length; k++) {
            if (visible[k].mid !== rep.mid) {
              var child = Object.create(visible[k]);
              child._isClusterChild = true;
              child._clusterId = cid;
              insert.push(child);
            }
          }
        }
      }
    }

    result.splice(entry.insertAt, 1);
    for (var k = insert.length - 1; k >= 0; k--) {
      result.splice(entry.insertAt, 0, insert[k]);
    }
    var delta = insert.length - 1;
    for (var m = j + 1; m < order.length; m++) {
      if (order[m].insertAt > entry.insertAt) {
        order[m].insertAt += delta;
      }
    }
  }

  _displayList = result;
}

// state    -- fold-state hash (_seriesFoldState or _clusterFoldState)
// id       -- series-id or cluster-id
// idKey    -- '_seriesId' or '_clusterId'
// flagKey  -- '_isSeries' or '_isCluster'
// foldKey  -- '_seriesFolded' or '_clusterFolded'
function toggleFold(state, id, idKey, flagKey, foldKey) {
  var manual = state[id];
  var effective;
  if (manual === true || manual === false) {
    effective = manual;
  } else {
    for (var i = 0; i < _displayList.length; i++) {
      if (_displayList[i][idKey] === id && _displayList[i][flagKey]) {
        effective = _displayList[i][foldKey];
        break;
      }
    }
  }
  state[id] = !effective;
  buildDisplayList();
  renderPage();
}

function toggleSeriesFold(sid) {
  toggleFold(_seriesFoldState, sid, '_seriesId', '_isSeries', '_seriesFolded');
}

function toggleClusterFold(cid) {
  toggleFold(_clusterFoldState, cid, '_clusterId', '_isCluster', '_clusterFolded');
}

/* ── Rendering (builds only the current page's DOM nodes) ────── */

// Builds a single-file or directory link for a list of attachments
// (patches, events, texts). Returns '' when the list is empty.
function attachmentLink(list, dir, label, icon) {
  if (!list || list.length === 0) return '';
  var n = list.length;
  var href = n === 1
    ? dir + '/' + list[0].file
    : dir + '/' + list[0].file.replace(/\/[^/]+$/, '/');
  var text = n === 1 ? '1 ' + label : n + ' ' + label + 's';
  return '<a class="row-icon" href="' + escAttr(href) +
    '" title="' + escAttr(text) +
    '" aria-label="' + escAttr(text) + '">' + icon + ' </a>';
}

function buildRowElement(rpt) {
  var r = rpt.raw;
  var type = rpt.type;
  var subject = r.subject || '';
  var from = r.from || '';
  var priority = rpt.priority;
  var acked = r.acked || '';
  var owned = r.owned || '';
  var closeReason = r['close-reason'] || '';
  var role = r.role || '';
  var archivedAt = r['archived-at'] || '';
  var supersededBy = (r['superseded-by'] || [])[0] || null;
  var awaitingFlag = rpt.awaiting;
  var expiry = r.expiry || '';
  var isoDate = rpt.isoDate;
  var author = rpt.author;

  var closedBool = rpt.closed;
  var closedMap = {canceled: 'C', expired: 'E', superseded: 'S'};
  var closedTitle = {C: 'Canceled', E: 'Expired', S: 'Superseded', R: 'Resolved'};
  var flagA = acked ? 'A' : '-';
  var flagO = owned ? 'O' : '-';
  var flagC = closedMap[closeReason] || (closedBool ? 'R' : '-');
  var flagsStr = flagA + flagO + flagC;
  var flagsTitle = [flagA === 'A' ? 'Acked' : '',
                    flagO === 'O' ? 'Owned' : '',
                    closedTitle[flagC] || ''].filter(Boolean).join(', ');
  var label = _typeLabels[type] || type;

  var subjectEsc = escHtml(subject);
  var subjectHtml;
  if (closeReason === 'canceled' || closeReason === 'superseded') {
    subjectHtml = '<em><s>' + subjectEsc + '</s></em>';
  } else if (closedBool) {
    subjectHtml = '<em>' + subjectEsc + '</em>';
  } else {
    subjectHtml = subjectEsc;
  }
  var _srcType = boneConfig.sourceType || '';
  if (archivedAt && _srcType !== 'alias' && _srcType !== 'mailbox') {
    var titleAttr = supersededBy ? ' title="Superseded by: ' + escAttr(supersededBy.subject || 'another report') + '"' : '';
    subjectHtml = '<a href="' + escAttr(archivedAt) + '"' + titleAttr + ' target="_blank">' + subjectHtml + '</a>';
  }

  var seriesHtml = '';
  if (rpt._isSeries && rpt._seriesMembers && rpt._seriesMembers.length > 1) {
    var arrow = rpt._seriesFolded ? '\u25B6' : '\u25BC';
    var stitle = rpt._seriesFolded ? 'Unfold series' : 'Fold series';
    seriesHtml = '<a href="#" class="row-icon" data-action="series-fold" data-id="' +
      escAttr(rpt._seriesId) + '" title="' + escAttr(stitle) + '">' + arrow + '</a>';
  } else if (rpt._isCluster && rpt._clusterMembers && rpt._clusterMembers.length > 1) {
    var carrow = rpt._clusterFolded ? '\u25B6' : '\u25BC';
    var ctitle = (rpt._clusterFolded ? 'Unfold related cluster' : 'Fold related cluster') +
                 (rpt._clusterSummary ? ' (' + rpt._clusterSummary + ')' : '');
    seriesHtml = '<a href="#" class="row-icon" data-action="cluster-fold" data-id="' +
      escAttr(rpt._clusterId) + '" title="' + escAttr(ctitle) + '">' + carrow + '</a>';
  } else if (rpt._isSeriesChild || rpt._isClusterChild) {
    // Indent unfolded children past the parent's caret so the
    // parent/child relationship stays visible.
    seriesHtml = '<span class="row-icon child-indent"></span>';
  }

  var patchHtml = attachmentLink(r.patches, 'patches', 'patch file', '\uD83E\uDE79');

  var relatedHtml = '';
  // Union of all qualified-relation kinds: a click on this link should
  // filter on every report linked to the current one regardless of how
  // (resolves, supersedes, duplicates, related-to, and their inverses).
  // Include the current report's own message-id so the whole related
  // cluster surfaces together, not just its neighbours.
  var allRelated = [];
  var seenMids = {};
  var selfMid = r['message-id'] || '';
  if (selfMid) { seenMids[selfMid] = 1; allRelated.push(selfMid); }
  _relationKinds.forEach(function(kind) {
    var entries = r[kind];
    if (entries && entries.length) {
      entries.forEach(function(e) {
        var mid = e && e['message-id'];
        if (mid && !seenMids[mid]) { seenMids[mid] = 1; allRelated.push(mid); }
      });
    }
  });
  if (allRelated.length > 1) {
    relatedHtml = '<a class="secondary row-icon" href="#" data-action="related" data-mids="' +
      escAttr(allRelated.join(',')) +
      '" title="Filter related reports">\u21B3' + (allRelated.length - 1) + ' </a>';
  }

  var eventsHtml = attachmentLink(r.events, 'events', 'event file', '\uD83D\uDCC5');
  var textsHtml  = attachmentLink(r.texts,  'text',   'text file',  '\uD83D\uDCC4');

  var votesHtml = '';
  if (r.votes) {
    var vs = r.votes.split('/'), vscore = parseInt(vs[0] || '0', 10);
    var vcls = vscore > 0 ? 'vote-pos' : vscore < 0 ? 'vote-neg' : 'vote-zero';
    votesHtml = '<span class="vote-badge ' + vcls + '">' + escHtml(r.votes) + '</span>';
  }

  // Due cell (pre-computed, no post-render DOM walk needed)
  var dueHtml = '';
  var dueStyle = '';
  if (rpt.dueDays !== null) {
    var dl = rpt.deadline;
    var days = rpt.dueDays;
    var dueLabel = days < 0 ? Math.abs(days) + 'd. ago' : 'In ' + days + ' d.';
    if (days < 0) dueStyle = 'color:var(--pico-del-color, #c0392b);';
    else if (days <= 3) dueStyle = 'color:var(--pico-ins-color, #b8860b);';
    dueHtml = '<a href="#" data-action="search-deadline" data-val="' + escAttr(dl) + '" title="' + escAttr(dl) + '">' + escHtml(dueLabel) + '</a>';
  }

  var priLabels = {3: 'A', 2: 'B', 1: 'C'};
  var priTitles = {3: 'Priority A -- urgent and important',
                   2: 'Priority B -- urgent',
                   1: 'Priority C -- important'};
  var priLabel = priLabels[priority] || ' ';
  var priTitle = priTitles[priority] || 'No priority';
  var isMaint = role === 'maintainer';
  var authorInner = isMaint ? '<strong>' + escHtml(author) + '</strong>' : escHtml(author);
  var authorHtml = '<a href="#" data-action="search-from" data-val="' + escAttr(from) + '" title="' + escAttr(from) + '">' + authorInner + '</a>';

  var ownerAddr = owned || '';
  var ownerHtml = ownerAddr
    ? '<a href="#" data-action="search-owner" data-val="' + escAttr(ownerAddr) + '" title="' + escAttr(ownerAddr) + '">' + escHtml(rpt.ownerDisplay) + '</a>'
    : '';

  // Date cell with expiry handling
  var dateHtml = '';
  if (isoDate) {
    var dateLink = '<a href="#" data-action="search-date" data-val="' + escAttr(isoDate) + '">' + escHtml(isoDate) + '</a>';
    if (expiry) {
      dateHtml = '<small title="Expires on ' + escAttr(expiry) + '"><em>' + dateLink + '</em></small>';
    } else {
      dateHtml = '<small>' + dateLink + '</small>';
    }
  }

  var tr = document.createElement('tr');
  tr.innerHTML =
    '<td title="Filter by type"><mark data-action="isolate-type" data-type="' + escAttr(type) + '" style="cursor:pointer">' + escHtml(label) + '</mark></td>' +
    '<td title="' + priTitle + '" style="text-align:center">' + priLabel + '</td>' +
    '<td style="text-align:center;' + dueStyle + '">' + dueHtml + '</td>' +
    '<td title="' + escAttr(flagsTitle) + '" style="text-align:center;font-family:monospace;font-size:0.8rem;letter-spacing:0.1em">' + flagsStr + '</td>' +
    '<td style="text-align:center">' + rpt.replies + '</td>' +
    '<td class="secondary" title="' + escAttr(ownerAddr) + '">' + ownerHtml + '</td>' +
    '<td class="secondary">' + authorHtml + '</td>' +
    '<td>' + seriesHtml + patchHtml + eventsHtml + textsHtml + relatedHtml + votesHtml + (awaitingFlag ? '<span class="row-icon" title="Awaiting reply">\u231A </span>' : '') + subjectHtml + '</td>' +
    '<td title="Filter">' + dateHtml + '</td>';

  return tr;
}

function renderPage() {
  var total = _displayList.length;
  var start, end;

  if (pageSize > 0 && total > pageSize) {
    var totalPages = Math.ceil(total / pageSize);
    if (currentPage > totalPages) currentPage = totalPages;
    if (currentPage < 1) currentPage = 1;
    start = (currentPage - 1) * pageSize;
    end = Math.min(start + pageSize, total);
    renderPagination(currentPage, totalPages, total);
  } else {
    start = 0;
    end = total;
    renderPagination(0, 0, total);
  }

  var tbody = document.querySelector('tbody');
  var fragment = document.createDocumentFragment();
  for (var i = start; i < end; i++) {
    var tr = buildRowElement(_displayList[i]);
    tr.classList.toggle('stripe', (i - start) % 2 === 1);
    if (_displayList[i]._isSeriesChild || _displayList[i]._isClusterChild) {
      tr.style.backgroundColor = 'var(--pico-card-background-color, #f8f9fa)';
    }
    fragment.appendChild(tr);
  }
  tbody.innerHTML = '';
  tbody.appendChild(fragment);

  var tableFig = document.getElementById('reports-table');
  var emptyEl  = document.getElementById('empty-state');
  if (tableFig && emptyEl) {
    var isEmpty = total === 0;
    tableFig.style.display = isEmpty ? 'none' : '';
    emptyEl.style.display  = isEmpty ? '' : 'none';
  }

  document.getElementById('status').textContent =
    _filteredReports.length + '/' + boneConfig.total + ' reports';

  // Setup subject toggles for rendered rows only (not all 2000+)
  _setupToggles(tbody);
  requestAnimationFrame(_showTogglesIfNeeded);
}

/* ── Pagination ──────────────────────────────────────────────── */

function renderPagination(page, totalPages, totalVisible) {
  var el = document.getElementById('pagination');
  if (!el) return;
  if (totalPages <= 1) { el.innerHTML = ''; return; }

  var html = '<nav aria-label="Pagination" style="display:flex;align-items:center;' +
    'justify-content:center;gap:0.3rem;margin-top:0.8rem;font-size:0.85rem">';
  html += '<button ' + (page <= 1 ? 'disabled ' : '') +
    'onclick="goToPage(' + (page - 1) + ')" ' +
    'style="padding:0.2rem 0.5rem;margin:0">&lsaquo;</button>';

  var pages = compactPageRange(page, totalPages, 7);
  for (var i = 0; i < pages.length; i++) {
    var p = pages[i];
    if (p === '...') {
      html += '<span style="padding:0 0.2rem">\u2026</span>';
    } else {
      html += '<button onclick="goToPage(' + p + ')" ' +
        (p === page ? 'class="outline" aria-current="page" ' : '') +
        'style="padding:0.2rem 0.5rem;margin:0;' +
        (p === page ? 'font-weight:700;border-width:2px' : '') + '">' + p + '</button>';
    }
  }

  html += '<button ' + (page >= totalPages ? 'disabled ' : '') +
    'onclick="goToPage(' + (page + 1) + ')" ' +
    'style="padding:0.2rem 0.5rem;margin:0">&rsaquo;</button>';
  html += '</nav>';
  el.innerHTML = html;
}

function compactPageRange(current, total, maxButtons) {
  if (total <= maxButtons) {
    var r = [];
    for (var i = 1; i <= total; i++) r.push(i);
    return r;
  }
  var pages = [1];
  var left = Math.max(2, current - 1);
  var right = Math.min(total - 1, current + 1);
  if (left > 2) pages.push('...');
  for (var i = left; i <= right; i++) pages.push(i);
  if (right < total - 1) pages.push('...');
  pages.push(total);
  return pages;
}

function goToPage(p) {
  currentPage = p;
  renderPage();
  pushURL();
  var tbl = document.querySelector('figure');
  if (tbl) tbl.scrollIntoView({behavior: 'smooth', block: 'start'});
}

/* ── Lazy-load closed reports ────────────────────────────────── */

function loadClosedReports(callback) {
  if (closedLoaded) { if (callback) callback(); return; }
  if (closedLoading) return;
  closedLoading = true;
  document.getElementById('status').textContent = 'Loading closed reports\u2026';
  fetch(boneConfig.closedJsonUrl)
    .then(function(resp) { return resp.json(); })
    .then(function(data) {
      if (closedLoaded) return;
      var reports = data.reports || [];
      for (var i = 0; i < reports.length; i++) {
        var rpt = prepareReport(reports[i]);
        _allReports.push(rpt);
        if (rpt.type && allTypes.indexOf(rpt.type) === -1) {
          allTypes.push(rpt.type);
          activeTypes[rpt.type] = true;
        }
      }
      closedLoaded = true;
      closedLoading = false;
      computeClusters();
      updateStatusButtons();
      if (callback) callback();
    })
    .catch(function(err) {
      closedLoading = false;
      console.error('Failed to load closed reports:', err);
      document.getElementById('status').textContent = 'Failed to load closed reports.';
    });
}

/* ── URL ↔ state ─────────────────────────────────────────────── */

function buildURL() {
  var params = new URLSearchParams();
  var q = getSearchInput().value;
  if (q) params.set('q', q);
  var active = allTypes.filter(function(t) { return activeTypes[t]; });
  if (active.length !== allTypes.length) params.set('types', active.join(','));
  if (!onlyOpen || queryIncludesClosed(q)) params.set('open', '0');
  if (onlyAcked)    params.set('acked', '1');
  if (onlyOwned)    params.set('owned', '1');
  if (onlyAwaiting) params.set('awaiting', '1');
  var sortKeys = Object.keys(sortState);
  if (sortKeys.length > 0) {
    params.set('sort', sortKeys[0]);
    params.set('dir', sortState[sortKeys[0]]);
  }
  if (pageSize > 0 && currentPage > 1) params.set('page', String(currentPage));
  var qs = params.toString();
  return location.pathname + (qs ? '?' + qs : '');
}

function pushURL()    { history.pushState(null, '', buildURL()); }
function replaceURL() { history.replaceState(null, '', buildURL()); }

/* ── Button handlers ─────────────────────────────────────────── */

// Mirror the visual 'outline' (inactive) class into aria-pressed so
// the toggle buttons expose their state to assistive tech.
function syncPressed(btn) {
  btn.setAttribute('aria-pressed', String(!btn.classList.contains('outline')));
}

// Set a toolbar toggle's active state (visual class + ARIA) by id.
function setToggleState(id, active) {
  var btn = document.getElementById(id);
  btn.classList.toggle('outline', !active);
  syncPressed(btn);
}

// Keyboard activation for the sortable column headers (tabindex=0).
function thSortKey(e) {
  if (e.key === 'Enter' || e.key === ' ') {
    e.preventDefault();
    e.currentTarget.click();
  }
}

function toggleType(type, btn) {
  activeTypes[type] = !activeTypes[type];
  btn.classList.toggle('outline');
  syncPressed(btn);
  currentPage = 1;
  filterReports();
  pushURL();
}

function syncToolbarButtons() {
  document.querySelectorAll('.filters button[data-type]').forEach(function(btn) {
    btn.classList.toggle('outline', !activeTypes[btn.getAttribute('data-type')]);
    syncPressed(btn);
  });
}

// Build the per-type filter buttons from the loaded report types. The
// server used to emit these statically; we now build them from data so
// the HTML shell carries no report-derived markup.
function buildTypeFilters() {
  var container = document.getElementById('type-filters');
  if (!container) return;
  container.textContent = '';
  allTypes.forEach(function(t) {
    var btn = document.createElement('button');
    btn.setAttribute('data-type', t);
    btn.textContent = _typeLabels[t] || t;
    btn.addEventListener('click', function() { toggleType(t, btn); });
    syncPressed(btn);
    container.appendChild(btn);
  });
}

function isolateType(type) {
  var active = allTypes.filter(function(t) { return activeTypes[t]; });
  if (active.length === 1 && active[0] === type) {
    allTypes.forEach(function(t) { activeTypes[t] = true; });
  } else {
    allTypes.forEach(function(t) { activeTypes[t] = (t === type); });
  }
  syncToolbarButtons();
  currentPage = 1;
  filterReports();
  pushURL();
}

function toggleAcked(btn) {
  onlyAcked = !onlyAcked;
  btn.classList.toggle('outline');
  syncPressed(btn);
  currentPage = 1;
  filterReports();
  pushURL();
}

function toggleOwned(btn) {
  onlyOwned = !onlyOwned;
  btn.classList.toggle('outline');
  syncPressed(btn);
  currentPage = 1;
  filterReports();
  pushURL();
}

function toggleAwaiting(btn) {
  onlyAwaiting = !onlyAwaiting;
  btn.classList.toggle('outline');
  syncPressed(btn);
  currentPage = 1;
  filterReports();
  pushURL();
}

function toggleOpen(btn) {
  onlyOpen = !onlyOpen;
  btn.classList.toggle('outline');
  syncPressed(btn);
  currentPage = 1;
  if (!onlyOpen && !closedLoaded) {
    loadClosedReports(function() { filterReports(); pushURL(); });
  } else {
    filterReports();
    pushURL();
  }
}

var _filterTimer;
function onSearchInput() {
  clearTimeout(_filterTimer);
  currentPage = 1;
  _filterTimer = setTimeout(function() { filterReports(); replaceURL(); }, 120);
}

// Delegated click handler for tbody rows. Each interactive element carries
// a [data-action] attribute; we dispatch on it instead of inlining onclick.
// User-supplied values (mids, addresses, ids) live in data-* attributes,
// which removes the need for JS-string escaping inside HTML attributes.
function tbodyClick(e) {
  var target = e.target.closest('[data-action]');
  if (!target) return;
  e.preventDefault();
  var d = target.dataset;
  switch (d.action) {
    case 'series-fold':     toggleSeriesFold(d.id); break;
    case 'cluster-fold':    toggleClusterFold(d.id); break;
    case 'related':         showRelated('m:' + d.mids); break;
    case 'isolate-type':    isolateType(d.type); break;
    case 'search-from':     setSearch('f:' + d.val); break;
    case 'search-owner':    setSearch('o:' + d.val); break;
    case 'search-date':     setSearch('d:' + d.val + '..'); break;
    case 'search-deadline': setSearch('D:' + d.val); break;
  }
}

/* ── Sort ────────────────────────────────────────────────────── */

var sortState = {};

function sortTable(key) {
  var dir = sortState[key] === 'asc' ? 'desc' : 'asc';
  sortState = {};
  sortState[key] = dir;
  document.querySelectorAll('th[data-sort]').forEach(function(th) {
    th.classList.remove('asc', 'desc');
    th.removeAttribute('aria-sort');
  });
  var th = document.querySelector('th[data-sort="' + key + '"]');
  th.classList.add(dir);
  th.setAttribute('aria-sort', dir === 'asc' ? 'ascending' : 'descending');
  sortReports(key, dir);
  buildDisplayList();
  currentPage = 1;
  renderPage();
  pushURL();
}

/* ── Status buttons ──────────────────────────────────────────── */

function updateStatusButtons() {
  var hasAcked = false, hasOwned = false, hasAwaiting = false;
  for (var i = 0; i < _allReports.length; i++) {
    var rpt = _allReports[i];
    if (!hasAcked   && rpt.acked   !== '') hasAcked = true;
    if (!hasOwned   && rpt.owned   !== '') hasOwned = true;
    if (!hasAwaiting && rpt.awaiting) hasAwaiting = true;
    if (hasAcked && hasOwned && hasAwaiting) break;
  }
  document.getElementById('btn-acked').style.display   = hasAcked   ? '' : 'none';
  document.getElementById('btn-owned').style.display    = hasOwned   ? '' : 'none';
  document.getElementById('btn-awaiting').style.display = hasAwaiting ? '' : 'none';
}

/* ── Restore from URL ────────────────────────────────────────── */

function restoreFromURL() {
  var params = new URLSearchParams(location.search);
  var qVal = params.get('q') || '';
  getSearchInput().value = qVal;

  if (params.has('types')) {
    var allowed = params.get('types').split(',');
    allTypes.forEach(function(t) { activeTypes[t] = allowed.indexOf(t) !== -1; });
  } else {
    allTypes.forEach(function(t) { activeTypes[t] = true; });
  }
  document.querySelectorAll('.filters button[data-type]').forEach(function(btn) {
    btn.classList.toggle('outline', !activeTypes[btn.dataset.type]);
    syncPressed(btn);
  });

  // If the search query bypasses the Open filter via 'closed:true', any
  // 'open=0' in the URL is a side-effect of that override -- not a button
  // toggle. Keep the button on (default) so clearing the query restores
  // Open-only filtering automatically.
  var queryHasClosed = queryIncludesClosed(qVal);
  onlyOpen = queryHasClosed ? true : (params.get('open') !== '0');
  setToggleState('btn-open', onlyOpen);

  onlyAcked = params.get('acked') === '1';
  setToggleState('btn-acked', onlyAcked);

  onlyOwned = params.get('owned') === '1';
  setToggleState('btn-owned', onlyOwned);

  onlyAwaiting = params.get('awaiting') === '1';
  setToggleState('btn-awaiting', onlyAwaiting);

  document.querySelectorAll('th[data-sort]').forEach(function(th) {
    th.classList.remove('asc', 'desc');
    th.removeAttribute('aria-sort');
  });
  sortState = {};
  if (params.has('sort') && params.has('dir')) {
    var key = params.get('sort');
    var dir = params.get('dir');
    var th  = document.querySelector('th[data-sort="' + key + '"]');
    if (th && (dir === 'asc' || dir === 'desc')) {
      sortState[key] = dir;
      th.classList.add(dir);
      th.setAttribute('aria-sort', dir === 'asc' ? 'ascending' : 'descending');
    }
  } else if (boneConfig.columnsSort) {
    // Default sort from --html-columns-sort (date -> desc, else asc).
    var dkey = boneConfig.columnsSort;
    var ddir = dkey === 'date' ? 'desc' : 'asc';
    var dth  = document.querySelector('th[data-sort="' + dkey + '"]');
    if (dth) {
      sortState[dkey] = ddir;
      dth.classList.add(ddir);
      dth.setAttribute('aria-sort', ddir === 'asc' ? 'ascending' : 'descending');
    }
  }

  currentPage = params.has('page') ? parseInt(params.get('page'), 10) || 1 : 1;

  if ((!onlyOpen || queryHasClosed) && !closedLoaded) {
    loadClosedReports(function() { filterReports(); });
  } else {
    filterReports();
  }
}

/* ── Subject fold/unfold ─────────────────────────────────────── */
var _setupToggles, _showTogglesIfNeeded;
(function() {
  var style = document.createElement('style');
  style.textContent =
    /* nth-child(8) = the Subject column.  The table is table-layout:
       fixed (see the page CSS): Subject gets all the leftover width,
       and this rule clips whatever does not fit it. */
    'td:nth-child(8) { position: relative; white-space: nowrap; overflow: hidden; }' +
    'td:nth-child(8).expanded { white-space: normal; overflow: visible; }' +
    /* Neutralize Pico's [role=button] chrome: the ellipsis must look
       like plain text, not a framed button. */
    '.unfold { position: absolute; right: 0; top: 50%; transform: translateY(-50%);' +
    '  cursor: pointer; font-weight: 700; font-size: 1em;' +
    '  padding: 0.1em 0.4em 0.1em 0.6em; user-select: none; z-index: 1;' +
    '  background-color: inherit; border: none; color: inherit;' +
    '  line-height: inherit; display: inline; }';
  document.head.appendChild(style);

  _setupToggles = function(container) {
    container.querySelectorAll('td:nth-child(8)').forEach(function(td) {
      if (td.querySelector('.unfold') || td.classList.contains('expanded')) return;
      var toggle = document.createElement('span');
      toggle.className = 'unfold';
      toggle.textContent = '\u2026';
      toggle.style.display = 'none';
      // Focusable and key-activatable: the ellipsis acts as a button.
      toggle.setAttribute('role', 'button');
      toggle.setAttribute('tabindex', '0');
      toggle.setAttribute('aria-label', 'Show full subject');
      toggle.onmousedown = function(e) {
        e.preventDefault();
        e.stopPropagation();
      };
      toggle.onclick = function(e) {
        e.preventDefault();
        e.stopPropagation();
        td.classList.add('expanded');
        toggle.remove();
      };
      toggle.onkeydown = function(e) {
        if (e.key === 'Enter' || e.key === ' ') toggle.onclick(e);
      };
      td.appendChild(toggle);
    });
  };

  _showTogglesIfNeeded = function() {
    var toggles = document.querySelectorAll('td:nth-child(8) .unfold');
    // Only processes rendered rows (current page), not all 2000+
    var items = [];
    for (var i = 0; i < toggles.length; i++) {
      var toggle = toggles[i];
      var td = toggle.parentElement;
      if (td.textContent.length < SUBJECT_TRUNCATE_MIN_CHARS) {
        toggle.style.display = 'none';
      } else {
        items.push({toggle: toggle, td: td});
      }
    }
    // Batch geometry reads (single reflow)
    var truncated = new Array(items.length);
    for (var i = 0; i < items.length; i++) {
      truncated[i] = items[i].td.scrollWidth > items[i].td.clientWidth + 1;
    }
    // Batch writes
    for (var i = 0; i < items.length; i++) {
      items[i].toggle.style.display = truncated[i] ? '' : 'none';
    }
  };

  window.addEventListener('resize', function() {
    requestAnimationFrame(_showTogglesIfNeeded);
  });
})();

/* ── Initialize ──────────────────────────────────────────────── */

// Distinct report types in first-appearance order. Matches the old
// server-side `(distinct (map type reports))`, which followed report date.
function collectTypes() {
  allTypes = [];
  for (var i = 0; i < _allReports.length; i++) {
    var t = _allReports[i].type;
    if (t && allTypes.indexOf(t) === -1) allTypes.push(t);
  }
  activeTypes = {};
  allTypes.forEach(function(t) { activeTypes[t] = true; });
}

// Consume the fetched all-open.json payload: prepare reports, derive the
// type set, fill in the counts the renderer needs, build the filter
// buttons, then restore UI state from the URL.
function initOpenReports(data) {
  data = data || {};
  var reports = data.reports || [];
  console.time('bone:prepare');
  for (var i = 0; i < reports.length; i++) {
    _allReports.push(prepareReport(reports[i]));
  }
  collectTypes();
  computeClusters();
  console.timeEnd('bone:prepare');

  function numOr(v, fallback) { return v != null ? v : fallback; }
  boneConfig.total       = numOr(data.total,           _allReports.length);
  boneConfig.openCount   = numOr(data['open-count'],   _allReports.length);
  boneConfig.closedCount = numOr(data['closed-count'], 0);

  buildTypeFilters();

  if (data.generated) {
    var g = document.getElementById('generated-at');
    if (g) g.textContent = 'Generated ' + data.generated;
  }

  console.time('bone:initial-render');
  restoreFromURL();
  console.timeEnd('bone:initial-render');
  updateStatusButtons();
}

// Single delegated listener on tbody (the node persists across renders;
// only its children are replaced).
document.querySelector('tbody').addEventListener('click', tbodyClick);
window.addEventListener('popstate', function() { restoreFromURL(); });

document.addEventListener('keydown', function(e) {
  if (e.key === '/' && !e.ctrlKey && !e.metaKey && !e.altKey) {
    var tag = (e.target.tagName || '').toLowerCase();
    if (tag === 'input' || tag === 'textarea' || tag === 'select') return;
    e.preventDefault();
    getSearchInput().focus();
  }
});

document.getElementById('status').textContent = 'Loading reports…';
fetch(boneConfig.openJsonUrl)
  .then(function(resp) { return resp.json(); })
  .then(initOpenReports)
  .catch(function(err) {
    console.error('Failed to load open reports:', err);
    var s = document.getElementById('status');
    if (s) s.textContent = 'Failed to load reports.';
  });
