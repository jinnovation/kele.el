# Contributor Guide

!!! info

    This page is a work-in-progress. More to come!

Thanks for your interest in contributing to Kele! We're glad to have you. :sunglasses:

This page will give you all (well, most) of the information you'll need to get
started.

## Documentation

Every symbol should be documented. This applies to both "public"
(`kele-function-name`) and "private" (`kele--function-name`) symbols.

Kele's documentation loosely follows the [Diataxis] framework.

All new functionality should have corresponding user-facing
documentation. Function docstrings are not a substitute for proper user-facing
tutorials, usage instructions, etc.

### Changelog

We [keep a changelog] for Kele. Any change that is user-facing should be called
out in the changelog.

### Architecture Decision Records (ADRs)

We use [architecture decision records (ADRs)](./references/adrs/index.md) in Kele **very sparingly**. Not every design
decision made warrants an ADR of its own. As a contributor, you may be asked to write an ADR for your contribution, but
only if it materially impacts Kele's user-facing behavior.

## Testing

We use [Buttercup] to write tests.

All PRs should have corresponding unit/integration tests. I am far more likely
to review and generally consider your PR if it has tests.

[Diataxis]: https://diataxis.fr
[Buttercup]: https://github.com/jorgenschaefer/emacs-buttercup
[keep a changelog]: https://keepachangelog.com/
