# AGENTS.md

## Commands

- Run tests with: `make test`

## General Practices

- Find package dependencies + their APIs in `.cask/` or `~/.emacs.d/`
- When adding new user-facing functionality, add a new corresponding record to CHANGELOG.md
- In general, base your implementations and designs off of the Magit package. Its implementation can
  be found on GitHub.
- Use stdlib Emacs packages, functions, and interfaces wherever possible.
- All messages intended for display to the user should be prefixed with `[kele]` followed by a space
