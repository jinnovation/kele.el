# 🥤 kele.el (Kubernetes Enablement Layer for Emacs)

[![MELPA](https://melpa.org/packages/kele-badge.svg)](https://melpa.org/#/kele)
[![MELPA Stable](https://stable.melpa.org/packages/kele-badge.svg)](https://stable.melpa.org/#/kele)
[![License](https://img.shields.io/github/license/jinnovation/kele.el)](https://github.com/jinnovation/kele.el/blob/main/LICENSE)

![](./img/kele.jpg)
![](./img/demo.gif)

Kele (*kě lè*, or *kə-ˈlə*) ("Kubernetes Enablement Layer for Emacs") is a
Kubernetes cluster management package. It empowers you to perform operations as
coarse or fine-grained as you need, **fast**, and get back to your work. See
[How-Tos > Usage](./how-tos/usage.md) for an overview of what's possible with
Kele.

Kele also comes with "batteries included." It contains several
[integrations](./how-tos/integrations.md) with noteworthy packages,
e.g. [Embark], that you can take advantage of in your own configs.

!!! tip

    To learn more about how Kele compares to some other Kubernetes packages for
    Emacs, see: [Explanations > Comparisons with Similar
    Packages/Tools](./explanations/comparisons.md).

!!! note

    Kele is not an official Kubernetes project.

## Getting Started

=== "Use-package + Straight"

    ```emacs-lisp
    (use-package kele
      :straight t
      :config
      (kele-mode 1))
    ```

=== "Use-package"

    ```emacs-lisp
    (use-package kele
      :config
      (kele-mode 1))
    ```

=== "Straight"

    ```emacs-lisp
    (straight-use-package kele)
    (kele-mode 1)
    ```

=== "The Hard Way"

    Clone this repository and all dependencies and put them in your load-path.

    ```emacs-lisp
    (require 'kele)
    (kele-mode 1)
    ```

## Design Ethos

Kele aims to have **PLANS**. Namely, it aims to be:

- **P**erformant: if it is easier/faster for you to simply use `kubectl` to get
  the info you need instead of Kele, then Kele has failed;
- **Lightweight**: minimally intrusive, minimize context-switching, keep
  "embellishments" to a minimum;
- **A**gile: get you the answers you need -- and let you return to your other
  work -- as quickly as possible;
- **N**imble: get you as coarse/detailed insight as needed at any given point,
  with no compromise to any of the other tenets listed here;
- **S**mart: provide sensible defaults and interfaces that "just make sense".

## About the Name

The name Kele comes from the Mandarin term for cola, 可乐 (*kě lè*). It is
also an abbreviation of "Kubernetes Enablement Layer for Emacs."

[Embark]: https://github.com/oantolin/embark
