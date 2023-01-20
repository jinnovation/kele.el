# Customization

This page outlines the customization options available to you in Kele.

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

[managed-fields]: https://kubernetes.io/docs/reference/using-api/server-side-apply/#field-management
[last-applied-config]: https://kubernetes.io/docs/tasks/manage-kubernetes-objects/declarative-config/#how-to-create-objects
