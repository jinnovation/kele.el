# Comparisons with Similar Packages/Tools

This page compares Kele with some of its peer packages.

## [kubernetes-el]

!!! note

    The author of Kele is a co-maintainer of [kubernetes-el].

[kubernetes-el] is a Kubernetes cluster management package for Emacs. It draws
**heavy** inspiration from [Magit], from its "status page"-centric interface
design down to its prevalent use of [Transient]-based keybindings.

### Design Philosophy

Kele draws heavy inspiration from [kubernetes-el]. It takes lessons learned
during [kubernetes-el] development and strives for a cluster management
experience that is lighter-weight, more agile, and more fluidly integrated with
the native Emacs environment.

Kele aims for a Kubernetes cluster management experience that is less intrusive,
requires less context-switching, and is overall more performant than
[kubernetes-el].

### Permissions

`kubernetes-el` assumes a high level of privilege over the managed clusters,
e.g. the ability to list namespaces. This is a simplifying assumption that
obviously does not hold in all cases.

Kele learns from this limitation by dynamically enabling/disabling specific
keybindings depending on reported permissions from the cluster under
management. For example, if you do not have permission to list Pods, then the
`l` keybinding within the `kele-resource` Transient prefix will be disabled.

### Support for custom resources

One of `kubernetes-el`'s biggest limitations is [its lack of support for custom
resources][kubernetes-el-69]. This limitation imposes a **very** low ceiling on
the package's utility. The root causes extend to `kubernetes-el`'s **hard-coded
and incomplete support for the Kubernetes core API** (see
[`kubernetes-el/kubernetes-el#306`][kubernetes-el-306]). Overhauling the
associated design decisions would amount, in my co-maintainer's opinion, to a
complete rewrite of `kubernetes-el` -- hence my decision to kick off development
on Kele.

Kele was implemented to support custom resources from day one. To the fullest
extent possible, "core" resources (Pods, Deployments, etc.) are treated in
exactly the same way as custom resources. Almost all resource-specific
functionality, e.g. display logic and commands, are implemented generically.

## [kubel]

[kubel] is a similar "UI-centric" cluster management package to
[kubernetes-el]. Its advantage over [kubernetes-el] is its accommodation of
users with limited privilege/permissions within the clusters in question.

Similar to [kubernetes-el], Kele focuses on providing a cluster management
experience that is more "piecemeal" ("get this targeted piece of information as
quickly as possible and move on with your life") and requires less
context-switching -- unavoidable with a status-page-centric user interface --
than [kubel].

[kubernetes-el]: https://kubernetes-el.github.io/kubernetes-el
[kubel]: https://github.com/abrochard/kubel
[Magit]: https://magit.vc
[Transient]: https://github.com/magit/transient
[kubernetes-el-69]: https://github.com/kubernetes-el/kubernetes-el/issues/69
[kubernetes-el-306]: https://github.com/kubernetes-el/kubernetes-el/issues/306
