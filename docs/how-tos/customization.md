# Customization

This page outlines the customization options available to you in Kele.

## Keybindings

### Defining a keybinding prefix

Kele comes with a spread of recommended keybindings predefined as part of
`kele-command-map`. `kele-command-map` is not assigned to a keybinding by
default, so as to be minimally disruptive to your personal configurations.

Using `kele-command-map`, you can choose your own keybinding to act as the
keybinding **prefix** for all of Kele's keybindings.

To bind, say, `s-k` as the keybinding prefix:

```emacs-lisp
(define-key kele-mode-map (kbd "s-k") kele-command-map)
```

Now you can use, for example, `s-k c` to access [context-related
commands](./usage.md#contexts). Give it a try!

## Interface

### Resource display

When [displaying a single resource](./usage.md#displaying-a-single-resource)
with `kele-get`, `kele-get` retrieves the **full** manifest for the requested
resource. This may include "noisy" sub-fields like
[`.metadata.managedFields`][managed-fields] or the
[`kubectl.kubernetes.io/last-applied-configuration`
annotation][last-applied-config] that distract from the "important" bits.

You can routinely filter out such fields using the `kele-filtered-fields` custom
variable. For example, to filter out both of the above:

```emacs-lisp
(setq kele-filtered-fields
  '((metadata managedFields)
    (metadata annotations kubectl.kubernetes.io/last-applied-configuration)))
```

### Suppress keybinding instructions

The results buffer for `kele-get` prints out a header blurb outlining the
keybindings available to you. If this is distracting to you or offends your
minimalist tendencies, use `kele-get-show-instructions` to disable printing of
the `kele-get` result buffer's keybindings.

```emacs-lisp
(setq kele-get-show-instructions nil)
```

## Customizing cache behavior

Kele provides a handful of customization variables with which you can influence [cache
behavior](../explanations/design.md#caches).

### Change the discovery cache polling interval

If you'd like Kele to poll the discovery cache more or less frequently than the default, set
`kele-discovery-refresh-interval`, then disable and re-enable `kele-mode`.

```emacs-lisp
(setq kele-discovery-refresh-interval)
(kele-mode -1)
(kele-mode +1)
```

??? warning "Known Issue"

    There is a potential for Kele's copy of the discovery cache to have outdated (relative to clusters' "true" state)
    information, even immediately after a poll. This is due to Kele's polling interval being separate from `kubectl`'s
    own default TTL for the discovery cache, which is 10 minutes "lazily" (that is, the cache is invalidated and its
    contents re-pulled at the next `kubectl` invocation after 10 minutes).

    Given Kele's extensive usage of `kubectl` under the hood, this should rarely present an actual issue to you as a
    user. If it does, consider pegging the value of `kele-discovery-refresh-interval` to hover roughly around 10
    minutes. The default value of `kele-discovery-refresh-interval` is 600 seconds, i.e. 10 minutes.

[managed-fields]: https://kubernetes.io/docs/reference/using-api/server-side-apply/#field-management
[last-applied-config]: https://kubernetes.io/docs/tasks/manage-kubernetes-objects/declarative-config/#how-to-create-objects
