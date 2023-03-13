# Design

This page goes into some of the design particulars of Kele. As an end user, feel free to explore at your curiosity, but
rest assured that none of the information here is strictly necessary for your successful use of Kele.

!!! tip

    This page may at times contain forward-looking statements, e.g. of design
    details that are upcoming but have not yet made their way into the main
    branch.

## Caches

Kele revolves around two main in-memory caches:

- One that maintains the `kubeconfig` contents (`kele--kubeconfig`);
- One that maintains the discovery cache in-memory (`kele--discovery-cache`).

Both of these caches are populated [asynchronously][async] on `kele-mode` initialization.

The kubeconfig cache is kept in sync via [a file watcher][file watches], which allows Kele to only incur read costs when
they're actually needed. This is particularly useful for the `kubeconfig` cache, since the Kubeconfig itself is only
occasionally modified in response to discrete user events, such as switching context or the default namespace for a
given context. In combination with asynchronous IO via the [`async`][async] package, Kele is able to keep itself in sync
with the underlying Kube configurations and caches without deadlocking users' Emacs environment.

On the other hand, the discovery cache is timer-based and pulls the contents of the discovery cache from the filesystem
at a set interval, as dictated by `kele-discovery-refresh-interval`.

!!! info "Why?"

    We elect not to use file-watchers for the discovery cache due to the risk of completely exhausting the file
    descriptors Emacs can use. For more details, see [`kele-01`](../adrs/01-timer-based-discovery-cache.md).

[file watches]: https://www.gnu.org/software/emacs/manual/html_node/elisp/File-Notifications.html
[async]: https://github.com/jwiegley/emacs-async/
