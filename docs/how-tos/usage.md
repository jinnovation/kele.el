# Usage

!!! note

    You'll notice this page is pretty sparse. Kele is an early-stage package
    with lots of room to grow. Stay tuned for more!

!!! note

    Kele doesn't have a default keybinding prefix for its commands. All examples
    documented here assume that you've opted for `s-k`.

    For instructions on how to set your own keybinding prefix, see: [How-Tos >
    Customization](./customization.md).

## Dispatch

| Keybinding | Interactive function |
|:-----------|:---------------------|
| `s-k ?`    | `kele-dispatch`      |

Kele provides `kele-dispatch` as a launchpad for all subsequent Kele
functionality. If you ever forget what the keybinding is for what you're trying
to accomplish, reach for `kele-dispatch`.

## Working with Resources

### Displaying a single resource

```
M-x kele-get
```

??? example "Demo"

    ![](./img/kele-get.gif)

!!! tip inline ""

    `kele-get` supports [custom resources] too!

`kele-get` allows you to retrieve the manifest for a given Kubernetes object and
displaying it in a separate buffer. `kele-get` will present you with completion
candidates for:

- The resource kind;
- The group-version (if the same resource kind exists in multiple groups);
- The namespace (if the resource kind is namespaced), and finally;
- The name of the resource itself.

!!! tip ""

    `kele-get` only shows you resource types that support `get`ting in the first
    place.

### Refreshing a resource

You can press `U` in a `kele-get` buffer to re-fetch and refresh the current resource.

## Contexts

| Keybinding | Interactive function |
|:-----------|:---------------------|
| `s-k c`    | `kele-context`       |

### Switching contexts

| Keybinding | Interactive function  |
|:-----------|:----------------------|
| `s-k c s`  | `kele-context-switch` |

??? example "Demo"

    ![](./img/context-switch.gif)

This can also be done via [Embark] on any selection candidate in any other
context-related Kele command, e.g. [`kele-context-rename`](#renaming-a-context).

### Renaming a context

| Keybinding | Interactive function  |
|:-----------|:----------------------|
| `s-k c r`  | `kele-context-rename` |

??? example "Demo"

    ![](./img/context-rename.gif)

This can also be done via [Embark] on any selection candidate in any other
context-related Kele command, e.g. [`kele-context-switch`](#switching-contexts).

### Changing the default namespace

| Keybinding | Interactive function                        |
|:-----------|:--------------------------------------------|
| `s-k c n`  | `kele-namespace-switch-for-current-context` |

### Managing proxy servers

Kele allows for starting and stopping [HTTP
proxies](https://kubernetes.io/docs/tasks/extend-kubernetes/http-proxy-access-api/)
for each context. The status of each context's proxy is displayed in the
annotations for each cluster completion candidate.

??? example "Demo"

    ![](./img/proxy-status-completion.png)

!!! note

    Any proxy server created via Kele is **ephemeral**; they are automatically
    closed and terminated after a set amount of time. For more details, see
    `kele-proxy-ttl`.

!!! note

    Each context can only have one proxy server active at a time. This is an
    artificial limitation put in place by Kele.

`M-x kele-proxy-start`
: Start a proxy server process for a given context

`M-x kele-proxy-stop`
: Stop a proxy server process for a given context

`M-x kele-proxy-toggle`
: Start or stop a proxy server process for a given context, depending on current status

[Embark]: https://github.com/oantolin/embark
[custom resources]: https://kubernetes.io/docs/concepts/extend-kubernetes/api-extension/custom-resources/
