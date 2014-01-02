Code Troll
==========

> ***troll.*** carefully and systematically search an area for something; an ugly cave-dwelling creature depicted as either a giant or a dwarf; a person who submits a deliberately provocative posting.

Code Troll is a command line tool that protects against intellectual property leaks in git commits.

How it works
------------

Codetroll works by doing a naive grep search across all files in the git repository.
It searches for a set of basic patterns (not regexes) to detect suspicious looking lines (i.e. where an intellectual property leak may be occurring).
A set of whitelist regexes are then applied to each match to filter out known-safe patterns.

If all detected matches are covered by the whitelists, then it will return with a status of 0.

If any of the detected matches are not covered by a whitelist, then it will return with a status of 1 and list all of the offending matches.

Patterns and Whitelists
-----------------------

Patterns are loaded from `~/.codetroll/patterns` (a single word per line to look for).

For example:

    aws_secret_key
    user_id
    password
    internal_server_domain_prefix

Whitelists are loaded from two locations: a per-repository whitelist (`<repo>/.codetroll-ignore`) and a set of user-specific whitelists (`~/.codetroll/whitelists/*`). 
Whitelists are specified as **tab-separated** files containing two columns:

1. **file regex**: this whitelist pattern will only apply to filenames matching this regex.
2. **code regex**: if this code regex matches for a given matched line (it is applied across the entire line - not just the matched pattern), it will be deemed safe.

For example:

    .*\.scala    package .*

Usage
-----

    codetroll [mode] [directory]

Codetroll can be run in two modes:

- `all`: all files in the git index are scanned.
- `modified`: only files that have been changed (i.e. in the current commit or working tree) are scanned.

The `directory` argument should be the absolute path to the git repository that should be scanned (i.e. `/Users/laurencer/src/codetroll`).

If a suspicious pattern is found, it should be added to the repository specific whitelist (`<repo>/.codetroll-ignore`) so that it is recorded as safe in an auditable location. 
Changes to this file should be code-reviewed.

**
Codetroll will exit with a status of 0 if no whitelists or patterns are found.
It is up to the user to ensure that the user whitelists and patterns are on the machine being used (i.e. through a compliance/enforcement tool).
**

The reason for this behaviour is to make Codetroll'd projects open-source friendly so that external contributors do not have to worry about what they commit.

Git Integration
---------------

Codetroll can be integrated with a Git pre-commit hook that runs codetroll.

The following script should be made executable and saved as `<repo>/.git/hooks/pre-commit`:

    #!/bin/sh
    #
    # Precommit hook that will run codetroll. 
    # It assumes that codetroll is installed on the user's path.

    which codetroll &>/dev/null
    if [ $? -eq 0 ]
    then
      REPO_DIR="$(dirname "$GIT_DIR")"
      codetroll modified $REPO_DIR
    else
      echo "Codetroll not found on path. Not checking commit for intellectual property leaks."
      exit 0
    fi
    
