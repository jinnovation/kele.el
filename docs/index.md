# kele.el

![img](docs/img/kele.jpg)

Kele (*kě lè*, or *kə-ˈlə*) ("Kubernetes Enablement Layer for Emacs")
streamlines integration between Kubernetes and Emacs. It provides a "base layer"
that can be leveraged to build higher-level integrations, e.g. modeline modules
and interactive clients such as
[kubernetes-el](https://github.com/kubernetes-el/kubernetes-el).

Kele comes with "batteries included." It contains several integrations with
noteworthy packages that you can take advantage of in your own configs. These
also serve to demonstrate Kele&rsquo;s capabilities. Our documentation includes
tutorials for replicating these integrations yourselves.

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

## About the Name

The name Kele comes from the Mandarin term for cola, 可乐 (*kě lè*). It is
also an abbreviation of "Kubernetes Enablement Layer for Emacs."


