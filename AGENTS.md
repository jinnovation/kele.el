# AGENTS.md

## Commands

- Run tests with: `make test`

## General Practices

- Find package dependencies + their APIs in `.cask/` or `~/.emacs.d/`
- When adding new user-facing functionality, add a new corresponding record to CHANGELOG.md. This
  can be found in the `docs/` directory.
- In general, base your implementations and designs off of the Magit package. Its implementation can
  be found on GitHub.
- Use stdlib Emacs packages, functions, and interfaces wherever possible.
- All messages intended for display to the user should be prefixed with `[kele]` followed by a space

## Debugging

- Perform as many operations as possible via `kele` Emacs functions. Only use "public" Emacs
  functions, i.e. functions prefixed with `kele-` instead of `kele--`. Avoid using `kubectl` unless
  absolutely necessary.
  - Public functions can be defined with `transient-define-suffix` in addition to `defun`.
- When printing buffer contents, use the `buffer-substring-no-properties` function. DO NOT use
  `buffer-string` unless specifically requested.
