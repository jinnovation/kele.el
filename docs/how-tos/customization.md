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

### YAML highlighting

Kele automatically detects and enables YAML highlighting for `kele-get` when
either:

- [`yaml-mode`](https://melpa.org/#/yaml-mode) is installed;
- The YAML [Treesitter grammar][treesitter] is installed, in which case the
  built-in `yaml-ts-mode` is used.

You can hard-code the major mode you'd like to use for YAML highlighting with
the `kele-yaml-highlighting-mode` variable.

If you'd prefer, you can also **disable** YAML highlighting by setting
`kele-yaml-highlighting-mode` to `nil`.

### Filtering out resource fields for display

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

### Changing resource caching expiration time

!!! note

    Currently only namespace names are cached.

Kele caches certain resource names upon fetching. This speeds up subsequent
queries drastically.

These cached values have a expiration time, after which the cached values are
erased.

You can change the default refresh interval with
`kele-resource-default-refresh-interval`. For example, a value of `60` means
that all cached values are erased 60 seconds after creation.

### Changing resource-specific caching expiration time

For some resources, you might expect the set of names in the cluster to change
more or less frequently than others. Some you might, for all intents and
purposes, assume **never** change.

You can set resource-specific cache expirations with
`kele-resource-refresh-overrides`. For example, the following will set cached
names for Pods to expire after 600 seconds:

```emacs-lisp
(setq kele-resource-refresh-overrides '((pod . 600)))
```

You can **also** set the special value `:never`, in which case the cached values
are **never** automatically erased once they're written. For example, the
following will set cached names for Namespaces to never expire:

```emacs-lisp
(setq kele-resource-refresh-overrides '((namespace . :never)))
```

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
[treesitter]: https://www.masteringemacs.org/article/how-to-get-started-tree-sitter
