;;; test-ui.el --- Test UI-based functionality. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(load-file "./tests/integration/undercover-init.el")

(require 'dash)
(require 'with-simulated-input)

(require 'kele)

(describe "Transient prefixes"
  (describe "kele-resource"
    (before-each
      (with-simulated-input
       "deployments RET"
       (call-interactively #'kele-resource)))
    (it "sets up fine lol"
      (display-warning 'buttercup
                       (format "buffers: %s" (-map #'buffer-name (buffer-list))))
      (expect t :to-be-truthy))))

;;; test-ui.el ends here

