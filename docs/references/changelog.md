# Changelog

All notable changes to this project will be documented here.

The format is based on [Keep a Changelog][],
and this project adheres to [semantic
versioning][semver].

[Keep a Changelog]: https://keepachangelog.com/en/1.0.0/
[semver]: https://semver.org/spec/v2.0.0.html

## Upcoming

### Added

- The primary update loop reading from kubeconfig and listening for changes on
  it is now asynchronous and therefore non-blocking!

### Fixed

- Fixed an issue where proxies were being created for the current context,
  regardless of which context it was actually requested for
- Fixed an issue in `kele-namespace-switch-for-context` where the selection
  candidates were pulled for the **current** context rather than the argument
  context
- Fixed an issue where attempting to pull completion candidates via queries to
  the proxy API server resulted in an error, as well as multiple proxy server
  processes being inadvertently spun up for the same context

### Changed

- Removed dependency: [`requests`](https://github.com/tkf/emacs-request)
- Added dependency: [`plz`](https://github.com/alphapapa/plz.el)
- Added dependency: [`async`](https://github.com/jwiegley/emacs-async/)

## 0.1.0

Kele is born!

This initial release of Kele has a very simple goal: "[`kubectx` and
`kubens`][1], but make it Emacs." Its feature set is limited but lays the
foundation -- both in terms of implementation and "design philosophy" -- for
future enhancements.

## Added

- Added ability to switch and rename contexts, with completion and caching
- Added ability to switch namespaces for any given context

[1]: https://github.com/ahmetb/kubectx
