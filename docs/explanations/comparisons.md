# Comparisons with Similar Packages/Tools

This page compares Kele with some of its peer packages.

!!! note

    Given Kele's relative infancy, this page compares less on concrete
    features and capabilities and more on design philosophy and overall
    goals. All packages listed here are, as of today, far more feature-complete
    than Kele.

## [kubernetes-el]

[kubernetes-el] is a Kubernetes cluster management package for Emacs. It draws
**heavy** inspiration from [Magit], from its "status page"-centric interface
design down to its prevalent use of [Transient]-based keybindings.

Kele itself draws inspiration from [kubernetes-el]. In fact, the author of Kele
is a co-maintainer of [kubernetes-el].

Kele draws from some lessons learned during [kubernetes-el] development and
strives for a cluster management experience that [has
PLANS](../index.md#design-ethos). More specifically, it aims for a Kubernetes
cluster management experience that is less intrusive, requires less
context-switching, and is overall more performant than [kubernetes-el]. If Kele
proves to be flexible enough that [kubernetes-el] could be re-implemented on top
of Kele, then that's a sign that we've done a good job here.

## [kubel]

[kubel] is a similar "UI-centric" cluster management package to
[kubernetes-el]. Its advantage over [kubernetes-el] is its accommodation of
users with limited privilege/permissions within the clusters in question.

Similar to [kubernetes-el], Kele focuses on providing a cluster management
experience that is more "piecemeal" ("get this targeted piece of information as
quickly as possible and move on with your life") and requires less
context-switching -- unavoidable with a status-page-centric user interface --
than [kubel].

[kubernetes-el]: https;//kubernetes-el.github.io/kubernetes-el
[kubel]: https://github.com/abrochard/kubel
[Magit]: https://magit.vc
[Transient]: https://github.com/magit/transient
