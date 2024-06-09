# Comparisons with Similar Packages/Tools

This page compares Kele with some of its peer packages.

!!! note

    Given Kele's relative infancy, this page compares less on concrete
    features and capabilities and more on design philosophy and overall
    goals. All packages listed here are, as of today, far more feature-complete
    than Kele.

## [kubernetes-el]

!!! note

    The author of Kele is a co-maintainer of [kubernetes-el].

[kubernetes-el] is a Kubernetes cluster management package for Emacs. It draws
**heavy** inspiration from [Magit], from its "status page"-centric interface
design down to its prevalent use of [Transient]-based keybindings.

Kele draws heavy inspiration from [kubernetes-el]. It takes lessons learned
during [kubernetes-el] development and strives for a cluster management
experience that is lighter-weight, more agile, and more fluidly integrated with
the native Emacs environment.

Kele aims for a Kubernetes cluster management experience that is less intrusive,
requires less context-switching, and is overall more performant than
[kubernetes-el].

One of `kubernetes-el`'s biggest limitations is [its lack of support for custom
resources][kubernetes-el-69]. This limitation imposes a **very** low ceiling on
the package's utility. The root causes extend to `kubernetes-el`'s **hard-coded
and incomplete support for the Kubernetes core API** (see
[`kubernetes-el/kubernetes-el#306`][kubernetes-el-306]). Overhauling the
associated design decisions would amount, in my co-maintainer's opinion, to a
complete rewrite of `kubernetes-el` -- hence my decision to kick off development
on Kele.

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
