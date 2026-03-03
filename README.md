# 🦴 BONE — Bark's Own Nifty Explorer

**bone** (what you get when you stop barking) is a standalone
[Babashka](https://github.com/babashka/babashka) script for browsing
[Bark](https://codeberg.org/bzg/bark) reports. When
[fzf](https://github.com/junegunn/fzf) is available, bone let you
browse Bark reports interactively.

## Install

Assuming [bbin](https://github.com/babashka/bbin) is installed:

    bbin install io.github.bzg/topics

## Configure

Create `~/.config/bone/config.edn`:

    {:email "you@example.com"}

By default, bone only shows reports where your email appears (as
author or the one who acked, owned, or closed). Use `-a` or `--all` to
see everything.

## Usage

    bone [options]

      -f, --file FILE            Read reports from a JSON file
      -u, --url URL              Fetch reports from a URL
      -U, --urls-file FILE       Fetch and merge from URLs listed in FILE
      -e, --email EMAIL          Your email (overrides config)
      -n, --source NAME          Filter by source name
      -p, --min-priority 1|2|3   Only show reports with priority >= N
      -s, --min-status 1-7       Only show reports with status >= N
      -a, --all                  Show all reports (not just yours)
      -                          Read JSON from stdin

## Examples

    # Browse your reports from a local file
    bone -f reports.json

    # Browse all reports from a remote URL
    bone -u https://example.com/reports.json -a

    # Merge reports from multiple BARK instances
    bone -U my-urls.txt

The URLs file (-U) lists one URL per line; blank lines and # comments
are ignored.

## License

Copyright © 2026 Bastien Guerry

Distributed under the Eclipse Public License 2.0.
