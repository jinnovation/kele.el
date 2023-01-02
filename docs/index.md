# 🥤 kele.el (Kubernetes Enablement Layer for Emacs)

![](./img/kele.jpg)

Kele (*kě lè*, or *kə-ˈlə*) ("Kubernetes Enablement Layer for Emacs") is a
Kubernetes cluster management package. It is structured around permitting you to
permit operations as coarse or fine-grained as you need, **fast**, and get back
to your work. See [How-Tos](./how-tos/index.md) for example usage.

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

### The Hard Way

Clone this repository and put it in your load-path.

### Use-package + Straight

```emacs-lisp
(use-package kele
  :straight (kele :type git :host github :repo "jinnovation/kele.el")
  :config
  (kele-mode 1))
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
