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

## [kubed]

Like Kele, [Kubed] is a Kubernetes management package for Emacs.

Functionally, there is considerable overlap in functionality between Kele and [Kubed]. I highly
encourage you to try both out and see which fits your workflow best! The differences between the two
packages lie primarily in architecture and design.

One of the biggest differences between Kele and Kubed is that the latter **avoids 3rd-party
dependencies**. This makes it that much more feasible to add Kubed to core Emacs in the future, if
desired. In general, Kubed aims to "make the most" out of core Emacs integrations. For users that
would like to minimize the number of transitive package dependencies they introduce to their
workflow, this aspect of Kubed can be appealing.

Conversely, Kele takes considerable liberty with its use of 3rd-party packages in the name of
providing extended functionality. For example, Kele leveragess
[`plz.el`](https://github.com/alphapapa/plz.el/) heavily in its direct communication with the
Kubernetes API via HTTP using ephemeral proxy processes. This approach differs considerably from the
other packages in this list, which by and large tend to rely on invoking the `kubectl` CLI in
sub-processes and parsing the resulting shell output. We believe that the HTTP-API-based approach is
not only more robust, but also allows for greater flexibility in the Emacs layer as a result. For
example, this proxy-based architecture enables its universal support for custom resources from day
one, treating them identically to core resources through generic implementation patterns.

Similarly, Kele's dynamic permissions detection allows it to gracefully adapt functionality based on
cluster-reported permissions, making it suitable for users with restricted cluster access while
maintaining its "piecemeal" philosophy of quickly retrieving targeted information with minimal
context-switching. Conversely, Kubed is based around "binding" package functionality to resources
explicitly with the `kubed-define-resource` function, which introduces minor friction to scaling
Kubed usage to all resources in a cluster.

[kubernetes-el]: https://kubernetes-el.github.io/kubernetes-el
[kubel]: https://github.com/abrochard/kubel
[kubed]: https://github.com/eshelyaron/kubed
[Magit]: https://magit.vc
[Transient]: https://github.com/magit/transient
[kubernetes-el-69]: https://github.com/kubernetes-el/kubernetes-el/issues/69
[kubernetes-el-306]: https://github.com/kubernetes-el/kubernetes-el/issues/306
