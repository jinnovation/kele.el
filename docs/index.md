---
hide:
  - navigation
---
# 🥤 kele.el (Kubernetes Enablement Layer for Emacs)

[![MELPA](https://melpa.org/packages/kele-badge.svg)](https://melpa.org/#/kele)
[![MELPA Stable](https://stable.melpa.org/packages/kele-badge.svg)](https://stable.melpa.org/#/kele)
[![License](https://img.shields.io/github/license/jinnovation/kele.el)](https://github.com/jinnovation/kele.el/blob/main/LICENSE)

Kele (*kě lè*, or *kə-ˈlə*) ("Kubernetes Enablement Layer for Emacs") is a
Kubernetes cluster management package. It empowers you to perform operations as
coarse or fine-grained as you need, **fast**, and get back to your work.

With Kele, you can:

- Manage [contexts], e.g. [switching, renaming, changing the default
  namespace](./how-tos/usage.md#contexts);
- [Fetch and display the manifest of a single
  resource](./how-tos/usage.md#working-with-resources);
- [Follow logs for a single resource](./how-tos/usage.md#following-logs)
- [List collections of resources](./how-tos/usage.md#working-with-resources);
- Start and stop [proxy servers](./how-tos/usage.md#managing-proxy-servers);
- Use the menu bar to perform basic cluster/config management;
- Add an [interactive modeline indicator](./how-tos/integrations.md#doom-modeline) with
  [`doom-modeline`](https://seagle0128.github.io/doom-modeline/);
- And more! :sunglasses:

See [How-Tos > Usage](./how-tos/usage.md) for more details on what's possible
with Kele.

!!! tip

    To learn more about how Kele compares to some other Kubernetes packages for
    Emacs, see: [Explanations > Comparisons with Similar
    Packages/Tools](./explanations/comparisons.md).

!!! note

    Kele is not an official Kubernetes project.

## Screenshots

=== "End-to-end"

    ![](./img/demo.gif)

=== "Menu bar integration"

    ![](./img/menu-bar.png)

=== "Resource listing"

    ![](./img/kele-list.png)

=== "`doom-modeline` integration"

    ![](./img/doom-modeline.png)

## Getting Started

!!! warning ""

    Kele requires Emacs 29+.

```emacs-lisp
(use-package kele
  :config
  (kele-mode 1)
  (bind-key (kbd "s-k") kele-command-map kele-mode-map))
```

## About the Name

The name Kele comes from the Mandarin term for cola, 可乐 (*kě lè*). It is
also an abbreviation of "Kubernetes Enablement Layer for Emacs."

[Embark]: https://github.com/oantolin/embark
[contexts]: https://kubernetes.io/docs/tasks/access-application-cluster/configure-access-multiple-clusters/
