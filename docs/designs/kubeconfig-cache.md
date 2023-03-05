# design-00: Kele Cache

!!! info "Status"

    WIP

This document outlines the design of the **"Kele Cache"**, aka the "K-cache" or
the "kache."

The kache brokers access to the Kubeconfig and the discovery cache. It is
designed for:

- Minimal memory overhead;
- Minimal query overhead.

## Status Quo

As of 2023-03-05, Kele uses a naive set of two separate caches:

- The "kubeconfig cache," which brokers reads from the user's configured
  kubeconfig (`kele-kubeconfig-path`);
- The "discovery cache," which brokers reads from the user's discovery cache.

!!! note

    The latter, confusingly, shares the same name with the discovery cache that
    lives in the user's filesystem, typically under
    `~/.kube/cache/discovery`. We use "discovery cache" to refer specifically
    to the Kele data structure, and use "filesystem discovery cache" to refer to
    the "actual" discovery cache.

On enablement of `kele-mode`, Kele initializes both the kubeconfig cache and the
discovery cache. The kubeconfig cache loads the contents of the user's
`kubeconfig` file into memory; likewise, the discovery cache loads the contents
of the user's filesystem discovery cache into memory. Both caches initialize
[file watchers] that "auto-refresh" the respective cache contents on changes to
the underlying file(s).

The combined use of these two caches enables near-instant completion of:

- A user's contexts and other cluster configurations;
- The available API groups, versions, and kinds on a given cluster.

## Problems

The aforementioned "all-at-once" approach has proven to have several
shortcomings.

### Gratuitous File-Watching

The most fundamental problem is that **the discovery cache does not
scale**. Emacs file-watching uses file descriptors under the hood, which Emacs
has a [finite number available for use at any given time][1]. Exceeding this
limit results in an error to the following effect:

```
File watching not possible, no file descriptor left: 975
```

This limit is, to my knowledge, **not** user-configurable. Even if it were, it
is an unreasonably invasive thing to ask users to do -- for an **Emacs package**
of all things.

Most notably, this limit is, like all things, shared globally within Emacs,
e.g. by [LSP-Mode][lsp-mode] and [auto-revert-mode], making the "real" limit for
Kele much lower.

In order to be a "good Emacs citizen," Kele needs to be much more conservative
and strategic in its use of file-watchers.

## Solution

## Design

## Known Issues

## Appendix

### References

[file watchers]: https://www.gnu.org/software/emacs/manual/html_node/elisp/File-Notifications.html
[1]: https://www.reddit.com/r/emacs/comments/mq2znn/no_file_descriptors_left/
[lsp-mode]: https://emacs-lsp.github.io/lsp-mode
[auto-revert-mode]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Auto-Revert.html
