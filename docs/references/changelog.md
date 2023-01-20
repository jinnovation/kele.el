# Changelog

All notable changes to this project will be documented here.

The format is based on [Keep a Changelog][],
and this project adheres to [semantic
versioning][semver].

[Keep a Changelog]: https://keepachangelog.com/en/1.0.0/
[semver]: https://semver.org/spec/v2.0.0.html

## Upcoming

### Added

- `kele-get` resource name input now supports completion! :fire:
- `kele-get` buffers now have keybindings for quitting and killing the resource
  display buffer
- You can now press `U` in `kele-get` buffers to re-fetch and refresh the
  current resource
- `kele-get` buffers' front matter now outlines the available keybindings

### Changed

- Added dependency: [`s`](https://github.com/magnars/s.el)

## 0.2.1

This release focuses on refining the `kele-get` experience -- usability
improvements and general bugfixes.

### Added

- `kele-get` buffers now have a dedicated minor mode, `kele-get-mode`.
- `kele-get` buffers now print a header detailing, for the resource under
  display: the **context** it was fetched from; and the **time of retrieval**
- Added a custom variable `kele-filtered-fields` with which you can routinely
  filter out resource fields from display in `kele-get`

### Fixed

- Fixed an issue where `kele-get` results buffer incorrectly prints namespace
  for un-namespaced resources as `nil`
- Fixed an issue where `kele-get` refused to display the retrieved resource, if
  a buffer corresponding to that resource already exists

### Changed

- `kele-get` completion for the resource type now only returns resources that
  support the `get` verb

## 0.2.0

This release introduces `kele-get` -- `kubectl get`, the Emacs way
:rocket:. With `kele-get` you can interactively specify the kind and name of the
resource that you'd like to `get` and display its manifest in a separate
buffer. What's more, it supports custom resources right out the gate -- a
[long-standing functionality gap in `kubernetes-el`][k8s-el-69].

See [How-Tos > Usage > Working with
Resources](../how-tos/usage.md#working-with-resources) for details and a demo
GIF. It's very much an MVP so there are some rough edges. Please open an [issue]
for any peculiar behavior that you notice.

### Added

- Implemented `kele-get` for interactively getting and displaying a given
  resource
- Context annotations now display whether a proxy server is currently active for
  the given context
- Implemented interactive functions for per-context proxy server management:
  `kele-proxy-start`, `kele-proxy-stop`, and `kele-proxy-toggle`

### Fixed

- Fixed an issue where disabling `kele-mode` resulted in an error reporting
  unbound slot `filewatch-id` on `kele--discovery-cache`
- Fixed an issue where cluster servers with ports,
  e.g. `https://127.0.0.1:51134`, were not recognized properly
- Fixed an issue where namespace completion candidates failed to populate on
  initial fetch of said namespaces from the Kubernetes API

### Changed

- Buffer for contexts' proxy processes are now hidden
- Default value for `kele-cache-dir` is now `~/.kube/cache`; before it was
  relative to the value of `kele-kubeconfig-path` (and unreliably so)

## 0.1.1

### Added

- Kubeconfig file watching is now asynchronous and therefore non-blocking!
- Kubeconfig file watching now prints a [progress report] denoting when changes
  were detected (and thus reading has begun asynchronously) and when reading has
  completed

### Fixed

- Fixed an issue where proxies were being created for the current context,
  regardless of which context it was actually requested for
- Fixed an issue in `kele-namespace-switch-for-context` where the selection
  candidates were pulled for the **current** context rather than the argument
  context
- Fixed an issue where attempting to pull completion candidates via queries to
  the proxy API server resulted in an error, as well as multiple proxy server
  processes being inadvertently spun up for the same context
- Fixed an issue where custom kubeconfig path is not respected by `kubectl`
  invocations

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

### Added

- Added ability to switch and rename contexts, with completion and caching
- Added ability to switch namespaces for any given context

[1]: https://github.com/ahmetb/kubectx
[progress report]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Progress.html
[k8s-el-69]: https://github.com/kubernetes-el/kubernetes-el/issues/69
[issue]: https://github.com/kubernetes-el/kubernetes-el/issues
