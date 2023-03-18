# adr-01: Timer-based discovery-cache caching

## Status

Implemented in [#150](https://github.com/jinnovation/kele.el/pull/150).

## Context

As of 2023-03-05, Kele uses a naive set of two separate caches:

- The "kubeconfig cache," which brokers reads from the user's configured kubeconfig (`kele-kubeconfig-path`);
- The "discovery cache," which brokers reads from the user's discovery cache.

!!! note

    The latter, confusingly, shares the same name with the discovery cache that
    lives in the user's filesystem, typically under
    `~/.kube/cache/discovery`. We use "discovery cache" to refer specifically
    to the Kele data structure, and use "filesystem discovery cache" to refer to
    the "actual" discovery cache.

On enablement of `kele-mode`, Kele initializes both the kubeconfig cache and the discovery cache. The kubeconfig cache
loads the contents of the user's `kubeconfig` file into memory; likewise, the discovery cache loads the contents of the
user's filesystem discovery cache into memory. Both caches initialize [file watchers] that "auto-refresh" the respective
cache contents on changes to the underlying file(s).

The combined use of these two caches enables near-instant completion of:

- A user's contexts and other cluster configurations;
- The available API groups, versions, and kinds on a given cluster.

## Problems

The aforementioned "all-at-once" approach has proven to have several shortcomings.

### Gratuitous File-Watching

The most fundamental problem is that **the discovery cache does not scale**. Emacs file-watching uses file descriptors
under the hood, which Emacs has a [finite number available for use at any given time][1]. Exceeding this limit results
in an error to the following effect:

```
File watching not possible, no file descriptor left: 975
```

This limit is, to my knowledge, **not** user-configurable. Even if it were, it is an unreasonably invasive thing to ask
users to do -- for an **Emacs package** of all things. Most notably, this limit is, like all things, shared globally
within Emacs, e.g. by [LSP-Mode][lsp-mode] and [auto-revert-mode], making the "real" limit for Kele much lower. In order
to be a "good Emacs citizen," Kele needs to be much more conservative and strategic in its use of file-watchers.

We note that it is **very easy** to hit this limit. Anecdotally, I maintain kubectl access to a handful of
production-scale clusters as part of my day job, and simply adding a couple more ad-hoc [Kind] clusters as part of
integration-testing for this very package is enough for me to hit the limit. Doing so requires me to manually delete the
filesystem discovery cache directories corresponding to these transient Kind clusters On top of being annoying, this is
also a fundamentally unreasonable workaround; what happens if a user simply **has that many clusters to maintain**?

## Decision

The simplest and "stupidest" solution to this problem is to make caching of the filesystem discovery cache timer-based
rather than filewatch-based.

## Consequences

This approach represents, to an extent, a "regression" of sorts in the functionality of the discovery cache, as now the
contents thereof are not guaranteed to be fully up-to-date with those of the filesystem dicovery cache. We decide that
this is safe, as the set of group-version-kinds present in a cluster are unlikely to change **that** frequently. This is
consistent with `kubectl`'s own refresh policy for the filesystem discovery cache, which refills the cache every ten
minutes -- and lazily, at that, i.e. on the next user invocation of `kubectl` after the ten-minute mark.
