# Contributing to BONE

This document is for people who want to read, modify, or extend the
BONE codebase.

## Reporting and patching

BONE uses its own conventions, on the project's own mailing list:

- Bug report: email
  [`~bzg/boneyard@lists.sr.ht`](mailto:~bzg/boneyard@lists.sr.ht) with
  subject `[BUG] bone: <short explicit description>`.
- Patch: email the same address with subject
  `[PATCH] bone: <commit summary>`.  Attach a `git format-patch` file
  or include the diff inline; either works.
- Feature request: subject `[FR] bone: <short description>`.
- General discussion: any other subject.

For private matters (security, personal context) you can write
directly to [bzg@bzg.fr](mailto:bzg@bzg.fr).

## Codebase

All database writes happen on the JVM side, in `src/`. The Babashka
scripts in `scripts/` are read-only.

- **JVM daemon** (`clj -M:run`) -- Connects to an IMAP server or a
  local Maildir, fetches new emails since the last run, digests them
  into reports, applies commands and roles, expires stale reports,
  exits.  With `--watch`, the process stays running and reacts to
  IMAP IDLE or Maildir filesystem events.

- **`bb export`** -- Reads the database and produces JSON, RSS, Org,
  ICS (calendar) feeds plus the HTML site and stats pages.

- **`bb notify`** -- Reads the database and sends notification
  emails to maintainers via SMTP.

### JVM namespaces (`src/bone/`)

| Namespace        | Role                                                 |
|------------------|------------------------------------------------------|
| `bone.main`      | Entry point (batch by default, `--watch` for IDLE)   |
| `bone.ingest`    | Datalevin connection, email parsing and storage      |
| `bone.digest`    | Single-email processing (classify, detect, thread)   |
| `bone.detect`    | Report type detection from subject + attachments     |
| `bone.commands`  | Commands (triggers, annotations) and votes           |
| `bone.roles`     | Permission checks and role management                |
| `bone.relations` | Qualified relations between reports                  |
| `bone.series`    | Patch series tracking                                |
| `bone.expire`    | Periodic report expiry (runs inside the daemon)      |
| `bone.periods`   | Per-source time-windowed config overrides            |
| `bone.tracking`  | Change-tracking for incremental export               |
| `bone.common`    | Shared utilities (JVM + Babashka)                    |
| `bone.config`    | config.edn loading, source map, DB path (JVM + bb)   |
| `bone.ics`       | iCalendar detection, parsing and assembly (JVM + bb) |
| `bone.logging`   | File and email log appenders                         |

`bone.common`, `bone.config` and `bone.ics` carry no Datalevin
dependency, so both the JVM and Babashka scripts load them. All other namespaces under `src/bone/`
are JVM-only.

### Babashka scripts (`scripts/`)

| Script                 | Role                                                  |
|------------------------|-------------------------------------------------------|
| `bone-export.clj`      | Report export (JSON, RSS, Org, patches, events, text) |
| `bone-notify.clj`      | Notification emails                                   |
| `bone-index.clj`       | HTML index page generation                            |
| `bone-stats.clj`       | Statistics and data page generation                   |
| `bone-docs.clj`        | Documentation page generation                         |
| `bone-maintenance.clj` | Orphan email purge                                    |
| `bone-email-test.clj`  | SMTP configuration test                               |
| `validate-config.clj`  | Config validation                                     |
| `bone`                 | Shell wrapper around the uberjar                      |

These scripts must never write to the Datalevin database.  Any
mutation goes through the JVM daemon.

## Development setup

Dependencies:

- [Clojure](https://clojure.org/) 1.12+ on **Java 17+** (the daemon
  needs `--add-opens` flags that are wired into `deps.edn`).
- [Babashka](https://babashka.org/) for the export, notify and
  maintenance scripts.
- [Datalevin](https://github.com/datalevin/datalevin): `deps.edn`
  pulls it in as a library; the bb scripts use it as a pod.

Clone the repository:

```sh
git clone https://codeberg.org/bzg/bone
cd bone
```

Run BONE locally:

```sh
cp config.edn.minimal config.edn      # or copy config.edn.example
$EDITOR config.edn                    # point :mailbox at your IMAP or Maildir
clj -M:run                            # single pass
clj -M:run -- --watch                 # stay running
```

To wipe the database and replay from scratch (prompts for
confirmation, destructive):

```sh
clj -M:run -- --fresh
```

## Running tests

Run the suite with `clj -M:test`.  Tests use `clojure.test` and live
in `test/bone/`; add a file `test/bone/<topic>_test.clj` to extend it
and the runner will pick it up.

Fixtures for integration tests live in `resources/emails.edn`.  When
you need to cover a new scenario in `digest_test.clj`, add a numbered
entry to that file with a brief header comment describing the case,
then reference it by message-id in the new assertion block.

## Debugging an ingest

When BONE does not digest an email as expected:

- Run `bb maintenance --failures` to list recent command-parsing
  errors and other ingest-time warnings recorded by the daemon.
- Enable verbose logging by adding a `:logging` block to `config.edn`
  (see `config.edn.example`); the per-email trace lands in the file
  you point it at.
- Reproduce the case as a fixture: add the email to
  `resources/emails.edn` and write an assertion in
  `test/bone/digest_test.clj`.  Faster than cycling through an IMAP
  fetch, and the case stays covered in CI.

## Code conventions

- Pure functions where possible.  `bone.common`, `bone.ics`, `bone.detect`,
  `bone.commands.registry` and the tx-builders in `bone.relations`
  must stay pure (no `datalevin.core` import).
- Side-effecting functions end with `!`.  Predicates end with `?`.
- Comments explain *why*, not *what*.  Add a short docstring to
  public functions whose intent is not obvious from the name.
- ASCII punctuation in source and prose: `--` for an em-dash,
  `=>` for a logical arrow, never the Unicode glyphs.
- Commit messages follow [conventional
  commits](https://www.conventionalcommits.org/en/v1.0.0/) and the
  50/72 git rule: a ~50-character summary, a blank line, then a
  short rationale focused on the *why*.

## Versioning

BONE follows [Intentional Versioning](https://intver.org).  The
three audiences considered for every release are:

- *Users* -- end-users and sysadmins who operate BONE instances.
- *Integrators* -- consumers of the exported JSON, RSS and Org feeds.
- *Maintainers* -- maintainers of the codebase itself.

A version bump signals which audience is most affected by the change;
the changelog calls out the impact per audience.

## License

The Clojure code is distributed under the [Eclipse Public License
2.0](LICENSES/EPL-2.0.txt). The JavaScript code shipped with the HTML
export is distributed under the [Mozilla Public License
2.0](LICENSES/MPL-2.0.txt).

By sending a patch, you agree to license your contribution under the
same terms.
