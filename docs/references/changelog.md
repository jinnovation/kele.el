# Changelog

All notable changes to this project will be documented here.

The format is based on [Keep a Changelog][],
and this project adheres to [semantic
versioning][semver].

[Keep a Changelog]: https://keepachangelog.com/en/1.0.0/
[semver]: https://semver.org/spec/v2.0.0.html

## Unreleased

### Added

- `kele-list` buffer now displays the Last Updated time
- Resource kind completion is now grouped by API group. For example, Deployments, DaemonSets, and
  ReplicaSets are all grouped under `apps` in the completion buffer.
- Resource name completion is now grouped by namespace for functions like `kele-get` or
  `kele-delete` that operate on a single resource.
- `kele-list` now uses collapsible sections with vtables inside, similar to Magit's status buffer.
  The resource kind name is used as the section heading.
- Added dependency on `magit-section`

### Fixed

- `kele-proxy-stop`: Interactive completion now filters out contexts without active proxy processes
- `kele-port-forward` is now inapt if the user does not have permission to create port-forwards
- `kele-resource-follow-logs` is now inapt if the user does not have permission to get pod logs

## 0.7.0

### Added

- `kele-list` now supports kind-specific columns. For example, listing
  Deployments will now show columns `READY`, `UP-TO-DATE`, and `AVAILABLE`,
  similar to `kubectl get deployments`. This is done via [server-side printing],
  meaning that you are guaranteed to see exactly the same thing as you'd get
  with `kubectl`
- `kele-resource` now has a keybinding to follow logs for resources that support
  it
- `kele-resource` now has a suffix for port-forwarding to resources that support
  it, with completion on the given resource's configured ports
- Implemented `kele-kill-port-forward` for terminating port-forwards that were
  created using `kele-port-forward`
- Rename `kele-proxy` to `kele-ports`

[server-side printing]: https://kubernetes.io/docs/reference/using-api/api-concepts/#receiving-resources-as-tables

### Fixed

- Fixed `kele-transient` using non-existent `kele-proxy` instead of `kele-ports`
- Context rename and delete operations now properly update all affected caches
- `kele-get` now correctly renders inapt for resources that don't support the
  `get` verb (e.g., `bindings`) (#81)
- `kele-list` now correctly renders inapt for resources that don't support the
  `list` verb
- `kele-resource` now no longer displays the resource-specific suffixes section if the chosen
  resource does not have any such suffix.

### Changed

- Renamed `kele-after-context-switch-hook` to
  `kele-context-after-switch-functions`. Each member of this variable is now
  expected to take the new context name as the sole argument (previously, they
  took no arguments).
- `kele-delete` now warns you when you're about to delete a "dangerous"
  resource, e.g. namespaces that can have unintended cascading effects when
  deleted

### Removed

- Removed unused dependency `ht`

## 0.6.0

### Added

- Implemented `kele-deployment-restart` for restarting Deployments.
- `kele-resource` now has a dedicated section for **kind-specific actions** that
  populates based on the resource kind you selected.
- Added ability to `delete` individual resources via `kele-resource` and
  `kele-list`
- Added variable for selecting which YAML major mode function to use for YAML
  highlighting in resource buffers
- Added output to `kele-list` buffers indicating the context
- Added additional columns to `kele-list` output, e.g. owner references
- In a `kele-list` table, `RET` now either opens the corresponding resource
  **or** the owning resource, depending on cursor position
- Added binding `g` for refreshing a `kele-list` buffer
- `kele-resource` now allows `--namespace=` to be unset. Resulting behavior is
  suffix-specific. For example, nil `--namespace=` for `kele-list` will list
  resources across all namespaces, while `kele-get` will do similarly for
  completion/selection.

### Fixed

- Fixed a bug where kubeconfig cluster entries with uppercase letters in the
  server address erroneously cause resource kind completion to silently fail and
  show no kinds present in cluster
- Fixed a bug in `kele-resource` where the improper singular/plural form of the
  resource name is used, e.g. "Get a single pods" instead of "Get a single pod"
- Fixed a bug where keybinding explanation "blurbs" in `kele-get` buffers don't
  properly show the keybinding in clickable form
- Fixed a bug where keybindings in `kele-get-mode` are not actually bound
- Fixed a bug where empty results when listing resources e.g. for `kele-list` or
  completion in `kele-get` results in false errors

### Changed

- Added dependency `memoize`
- The `kele-resource` suffixes now disable themselves if you don't have the
  required permissions. For example, if you don't have permission to `list`
  Pods, the `l` keybinding will be grayed out and inaccessible.
- Migrated `kele-list` to
  [`vtable.el`](https://www.gnu.org/software/emacs/manual/html_node/vtable/index.html)
- Bumped dependency `plz` from `0.7.3` to `0.8.0`

### Removed

- Removed support for Emacs 28

## 0.5.0

### Added

- **[Menu bar] integration**. Now you can access common tasks via the
  **Kubernetes** section on the menu bar
- Added ability to switch contexts from within the menu bar; available contexts
  are shown as a sub-menu
- Added ability to switch the default namespace of the current context from
  within the menu bar; available namespaces are shown as a sub-menu
- Added ability to specify that cached names of a specific resource should never
  expire

### Fixed

- Fixed a bug where a proxy server is sometimes initialized on a privileged
  port, resulting in TCP `permission denied` errors

### Changed

- Changed the default cache expiration time for namespaces from 600 seconds to
  **never**, since the set of namespaces in a cluster rarely if ever change. You
  can use `kele-cache-namespaces` to force-refresh them as needed.
- Namespace selection now checks whether or not you have permission to list
  namespaces from the cluster and falls back to verbatim string when you don't

## 0.4.2

### Fixed

- Fixed a bug where `kele-mode` errors out when the kubeconfig file does not
  exist yet
- Fixed a bug where `curl` errors are not properly caught, resulting in attempts
  to query the kubectl proxy server before it is ready

### Changed

- Bumped dependency `plz` from `0.3` to `0.7.3`

### Removed

- Removed integration with [`awesome-tray`](https://github.com/manateelazycat/awesome-tray)

## 0.4.1

### Fixed

- Fixed an issue where Kele can exhaust the number of file descriptors available for Emacs to use.
- Fixed an issue where command families that require proxy servers, e.g. `kele-resource`, result in false-positive
  timeout errors when starting up the proxy server.

### Changed

- Discovery cache polling is now timer-based instead of "dynamic," i.e. in response to filesystem changes.

## 0.4.0

Lots of fun stuff in this release.

Most importantly, Kele 0.4.0 introduces `kele-list` (`s-k <resource type> l`)
and -- with it -- the ability to list all resources of a given type in tabulated
form. Before, you could only fetch single resources. Now, with `kele-list`, you
can create ad-hoc "overviews" of specific resource types within a given context
and namespace; hitting Enter on any entry in this list brings up the resource's
full manifest.

Kele 0.4.0 also introduces the `kele-proxy` command palette (`s-k p`) for
starting/stopping proxy servers for contexts. It also fleshes out the Kubeconfig
management command palette (`s-k c`).

For more details, see: [How-Tos > Usage].

### Added

- Added a `kele-proxy` command prefix for managing proxy servers
- Added a keybinding to `kele-context` to enable/disable the proxy server for
  the current context
- Added a keybinding to `kele-context` for deleting a context
- Added a keybinding to open `kele-kubeconfig-path` in a buffer
- Added a keybinding to `kele-resource` to support listing out all resources of
  a given type (`kele-list`)

### Fixed

- `kele-context` and `kele-resource` now wait on kubeconfig sync completion to
  finish if one is currently in progress
- Fixed a bug where force-enabling or force-disabling `kele-mode` (via either
  `(kele-mode 1)` or `(kele-mode -1)`) when `kele-mode` is already active or
  inactive (respectively) resulted in errors
- Fixed an issue where attempting to invoke `s-k <resource name> g` sometimes
  results in the following error: `transient-setup: Suffix
  transient:kele-resource::command is not defined or autoloaded as a command`

### Changed

- Renamed `kele-context` to `kele-config`
- Increased the default value of `kele-proxy-ttl` from `60` to `180`
- Binding for `kele-get` in `kele-get-mode` changed from `U` to `g`

## 0.3.0

This release focuses on providing Kele's command palette and user interface with
a scalable foundation for future growth. As Kele's spread of user-facing
commands grows, it becomes less and less reasonable to expect users to `M-x`
everything.

To that end, this release adds the following:

- A Kele command keymap that will allow keybinding-based access to all
  Kele-based functionality (see [How-Tos > Customization] and [How-Tos > Usage]
  for more details)
- A spread of three [Transient]-based "prefix" commands -- `kele-context`,
  `kele-resource`, and `kele-dispatch` -- for nested command discovery and
  ad-hoc configuration, e.g. overriding the context and namespace to use for
  resource fetching. Again, see [How-Tos > Usage] for more details.

I'm optimistic that these two additions make Kele's user interface much more
pleasant and nimble, while also giving it ample room to grow in complexity and
scope in the coming releases.

### Added

- `kele-get` resource name input now supports completion! :fire:
- `kele-get` buffers now have keybindings for quitting and killing the resource
  display buffer
- You can now press `U` in `kele-get` buffers to re-fetch and refresh the
  current resource
- `kele-get` buffers' front matter now outlines the available keybindings
- Added a `kele-dispatch` command for when you forget the specific keybinding of
  what you're trying to accomplish
- Added a `kele-context` command prefix for context-related actions
- Added a `kele-resources` command prefix for acting on specific resource kinds,
  e.g. `get`ting, with support for selecting the context, namespace, and the
  group-version to use (in cases of ambiguity)

### Changed

- Added dependency: [`s`](https://github.com/magnars/s.el)
- Removed dependency: [`requests`](https://github.com/tkf/emacs-request)
- Increased minimum required Emacs version to 28.1

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
[How-Tos > Customization]:https://jonathanj.in/kele.el/how-tos/customization/
[How-Tos > Usage]: https://jonathanj.in/kele.el/how-tos/usage/
[Transient]: https://magit.vc/manual/transient.html
[Menu bar]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Menu-Bar.html
