# 🥤 kele.el (Kubernetes Enablement Layer for Emacs)

![](./img/kele.jpg)

Kele (*kě lè*, or *kə-ˈlə*) ("Kubernetes Enablement Layer for Emacs")
streamlines integration between Kubernetes and Emacs. It provides a "base layer"
that can be leveraged to build higher-level integrations, e.g. modeline modules
and interactive clients such as
[kubernetes-el](https://github.com/kubernetes-el/kubernetes-el).

Kele comes with "batteries included." It contains several
[integrations](./how-tos/integrations.md) with noteworthy packages that you can
take advantage of in your own configs. These also serve to demonstrate Keles's
capabilities.

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
