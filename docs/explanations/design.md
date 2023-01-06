# Design

This page goes into some of the design particulars of Kele. As an end user, feel
free to explore at your curiosity, but rest assured that none of the information
here is strictly necessary for your successful use of Kele.

!!! tip

    This page may at times contain forward-looking statements, e.g. of design
    details that are upcoming but have not yet made their way into the main
    branch.

## Caches

Kele revolves around two main in-memory caches:

- One that maintains the `kubeconfig` contents (`kele--kubeconfig`);
- One that maintains the discovery cache in-memory (`kele--discovery-cache`).

Both of these caches are populated asynchronously on `kele-mode`
initialization. They are kept in sync via [file watches], which allows Kele to
only incur read costs when they're actually needed. This is particularly useful
for the `kubeconfig` cache, since the Kubeconfig itself is only occasionally
modified in response to discrete user events, such as switching context or the
default namespace for a given context.

Notably, **all read operations within Kele are based primarily on these two
caches**. This helps keep filesystem read/write overhead at a minimum and keep
things snappy.

[file watches]: https://www.gnu.org/software/emacs/manual/html_node/elisp/File-Notifications.html
