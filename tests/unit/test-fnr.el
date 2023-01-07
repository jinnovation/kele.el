;;; test-fnr.el --- Tests for filenotify-recursive -*- lexical-binding: t; -*-(require 'buttercup)
;;; Commentary:
;;; Code:

(load-file "./tests/unit/undercover-init.el")

(require 'kele-fnr)

(defvar test-fnr--temp-testdir nil
  "Test root directory.")

(defvar test-fnr-event-counter (make-hash-table :test 'equal)
  ".")

(defun test-fnr-counter-increment (f)
  "."
  (if-let ((count (gethash f test-fnr-event-counter)))
      (puthash f (1+ count) test-fnr-event-counter)
    (puthash f 1 test-fnr-event-counter)))

(defun test-fnr-counter-reset ()
  "."
  (setq test-fnr-event-counter (make-hash-table :test 'equal)))

(defun fnr--make-temp-dir ()
  "Create a temporary file directory for test."
  (unless (stringp test-fnr--temp-testdir)
    (setq test-fnr--temp-testdir
          (expand-file-name
           (make-temp-name "fnr-test") temporary-file-directory)))
  (unless (file-directory-p test-fnr--temp-testdir)
    (make-directory test-fnr--temp-testdir)))

(defun test-fnr-create-test-directory ()
  (fnr--make-temp-dir)
  (make-directory (expand-file-name "a/a1" test-fnr--temp-testdir) 'parents)
  (make-directory (expand-file-name "b" test-fnr--temp-testdir) 'parents))

(defun test-fnr-touch (f)
     "Touches F."
     (interactive)
     (shell-command (concat "touch " (shell-quote-argument f))))

(describe "fnr--subdirectories-recursively"
  (before-each
    (test-fnr-create-test-directory)
    (test-fnr-counter-reset)
    (kele--fnr-clear-all))

  (it "correctly lists subdirectories"
    (expect (mapcar #'directory-file-name (kele--fnr-subdirectories-recursively test-fnr--temp-testdir))
            :to-have-same-items-as
            (mapcar #'directory-file-name `(,test-fnr--temp-testdir
                                           ,(expand-file-name "a" test-fnr--temp-testdir)
                                           ,(expand-file-name "a/a1" test-fnr--temp-testdir)
                                           ,(expand-file-name "b" test-fnr--temp-testdir))))))

(provide 'test-fnr)
;;; test-fnr.el ends here
