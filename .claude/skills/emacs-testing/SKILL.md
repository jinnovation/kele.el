---
name: emacs-testing
description: Test changes in a dedicated Emacs session. Good for validating behavior and UI results.
---

Before starting, you MUST ask me if you have permission to test in the existing Emacs server. If
not, start a new Emacs daemon server: `emacs --daemon=kele-testing`.

Send commands to the Emacs with: `emacsclient --eval`. If I did not give permission to run in the
existing server, use `emacsclient -s kele-testing --eval`.

If you created a separate daemon server, terminate it once you are done testing.
