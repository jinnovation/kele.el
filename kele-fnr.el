;;; kele-fnr.el --- filenotify, but recursive -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:
;;
;;  This is an extension of the built-in filenotify library, making it apply
;;  recursively.  It also maintains recursive watchers through the session.
;;
;;  Much thanks to Jethro Kuan (@jethrokuan) for the original
;;  implementation.  See: github.com/jethrokuan/filenotify-recursive.
;;
;;; Code:
(require 'filenotify)
(require 'cl-lib)

;;; Variables
(defvar kele--fnr-descriptors (make-hash-table :test 'equal)
  "Hash table for registered kele-fnr descriptors.
A key in this hashtable is a uuid.  The value in the hash table
is a `kele--fnr-watch' struct.")

;;; Utilities
(defun kele--fnr-uuid ()
  "Return string with random (version 4) UUID."
  (let ((rnd (md5 (format "%s%s%s%s%s%s"
                          (random)
                          (user-uid)
                          (emacs-pid)
                          (user-full-name)
                          user-mail-address
                          (recent-keys)))))
    (format "%s-%s-4%s-%s%s-%s"
            (substring rnd 0 8)
            (substring rnd 8 12)
            (substring rnd 13 16)
            (format "%x"
                    (logior
                     #b10000000
                     (logand
                      #b10111111
                      (string-to-number (substring rnd 16 18) 16))))
            (substring rnd 18 20)
            (substring rnd 20 32))))

(defun kele--fnr-subdirectories-recursively (dir &optional regexp predicate follow-symlinks)
  "Return list of subdirectories under directory DIR.
This function works recursively.  Files are returned in \"depth
first\" order, and files from each directory are sorted in
alphabetical order.  Each file name appears in the returned list
in its absolute form.


If REGEXP, when the directory matches REGEXP it is skipped.

PREDICATE can be either nil (which means that all subdirectories
of DIR are descended into), t (which means that subdirectories that
can't be read are ignored), or a function (which is called with
the name of each subdirectory, and should return non-nil if the
subdirectory is to be descended into).

If FOLLOW-SYMLINKS is non-nil, symbolic links that point to
directories are followed.  Note that this can lead to infinite
recursion."
  (let* ((result nil)
         (dir (directory-file-name dir)))
    (dolist (file (sort (file-name-all-completions "" dir)
                        'string<))
      (unless (or (member file '("./" "../"))
                  (and regexp
                       (string-match regexp file)))
        (when (directory-name-p file)
          (let* ((leaf (substring file 0 (1- (length file))))
                 (full-file (concat dir "/" leaf)))
            ;; Don't follow symlinks to other directories.
            (when (and (or (not (file-symlink-p full-file))
                           (and (file-symlink-p full-file)
                                follow-symlinks))
                       ;; Allow filtering subdirectories.
                       (or (eq predicate nil)
                           (eq predicate t)
                           (funcall predicate full-file)))
              (let ((sub-files
                     (if (eq predicate t)
                         (ignore-error file-error
                           (kele--fnr-subdirectories-recursively
                            full-file predicate follow-symlinks))
                       (kele--fnr-subdirectories-recursively
                        full-file predicate follow-symlinks))))
                (setq result (nconc result sub-files))))))))
    (cons dir result)))

(cl-defstruct (kele--fnr-watch (:constructor kele--fnr-watch-create)
                          (:copier nil))
  "Internal struct for managing filenotify recursive watchers.
UUID is a unique identifier string that's used as a key in
`kele--fnr-descriptors'.

FLAGS, CALLBACK and REGEXP are the same as in `kele--fnr-add-watch'
that used by each of the watcher.

DESCS is a list of cons cells, where each `car' corresonds to the
currently watched directory and `cdr' to descriptor returned by
each `filenotify' watcher to watch such directory."
  uuid
  flags
  regexp
  callback
  descs)

(defun kele--fnr-add-watchers (dirs flags callback)
  "Add file watcher with FLAGS and CALLBACK to each directory in DIRS.
Return back a list of descs cells (directory . descriptor)."
  (mapcar (lambda (dir)
            (cons dir (file-notify-add-watch dir flags callback)))
          dirs))

(defun kele--fnr-rm-watchers (descs)
  "Remove file watcher from a list of DESCS (directory . descriptor) cells."
  (dolist (cell descs) (file-notify-rm-watch (cdr cell))))

(defun kele--fnr-update-descs (watcher descs)
  "Set new DESCS for recursive WATCHER and update it in `kele--fnr-descriptors'."
  (setf (kele--fnr-watch-descs watcher) descs)
  (puthash (kele--fnr-watch-uuid watcher) watcher kele--fnr-descriptors))

;;;
(defun kele--fnr-add-watch (dir flags callback &optional regexp)
  "Create a new recursive watcher for filesystem events to DIR.
Use `kele--fnr-rm-watch' to cancel the watch.

The returned value is a UUID. If the file cannot be watched for
some reason, this function signals a `file-notify-error' error.

FLAGS is a list of conditions to set what will be watched for. It
can include the following symbols:

  `change'           -- watch for file changes
  `attribute-change' -- watch for file attributes changes, like
                        permissions or modification time


When any event happens, Emacs will call the CALLBACK function passing
it a single argument EVENT, which is of the form

  (DESCRIPTOR ACTION FILE [FILE1])

DESCRIPTOR is the same object as the one returned by this function.
ACTION is the description of the event.  It could be any one of the
following:

  `created'           -- FILE was created
  `deleted'           -- FILE was deleted
  `changed'           -- FILE has changed
  `renamed'           -- FILE has been renamed to FILE1
  `attribute-changed' -- a FILE attribute was changed
  `stopped'           -- watching FILE has been stopped

FILE is the name of the file whose event is being reported.

If REGEXP is non-nil, do not watch directories matching REGEXP."
  (let* ((uuid (kele--fnr-uuid))
         (all-dirs (kele--fnr-subdirectories-recursively dir regexp))
         (wrapped-callback (kele--fnr-wrap-callback uuid callback))
         (descs (kele--fnr-add-watchers all-dirs flags wrapped-callback))
         (watcher (kele--fnr-watch-create :uuid uuid
                                     :flags flags
                                     :descs descs
                                     :regexp regexp
                                     :callback wrapped-callback)))
    (puthash uuid watcher kele--fnr-descriptors)
    uuid))

(defun kele--fnr-rm-watch (uuid)
  "Remove recursive watcher by UUID."
  (let ((watcher (or (gethash uuid kele--fnr-descriptors)
                     (user-error "No watcher with id %s" uuid))))
    (kele--fnr-rm-watchers (kele--fnr-watch-descs watcher))
    (remhash uuid kele--fnr-descriptors)
    uuid))

(defun kele--fnr-wrap-callback (uuid callback)
  "Wraps the user-provided CALLBACK to include keeping track of new change.
UUID is the uuid of the kele--fnr-watcher."
  (lambda (event)
    (funcall #'kele--fnr-update-directory-watchers uuid event)
    (funcall callback event)))

(defun kele--fnr-update-directory-watchers (uuid event)
  "Update directories watched by UUID watcher by reacting to `filenotify' EVENT.
UUID corresponds to recursive watcher present in `kele--fnr-descriptors'."
  (let ((watcher (gethash uuid kele--fnr-descriptors)))
    (cl-destructuring-bind (_ action &rest files) event
      (when (and (memq action '(created stopped renamed))
                 (cl-loop for f in files
                          when (kele--fnr-directory-actionable-p watcher f) return t))
        (apply (intern (format "kele--fnr-update-%s-directory" action))
               watcher files)))))

(defun kele--fnr-update-created-directory (watcher root)
  "Using the recursive WATCHER, start watching new ROOT and its subdirectories."
  (let* ((new-dirs (kele--fnr-subdirectories-recursively root (kele--fnr-watch-regexp watcher)))
         (new-descs (kele--fnr-add-watchers new-dirs
                                       (kele--fnr-watch-flags watcher)
                                       (kele--fnr-watch-callback watcher)))
         (old-descs (kele--fnr-watch-descs watcher)))
    (kele--fnr-update-descs watcher (nconc new-descs old-descs))))

(defun kele--fnr-update-stopped-directory (watcher root)
  "Using the recursive WATCHER, stop watching ROOT and its subdirectories."
  (let* ((old-descs (kele--fnr-watch-descs watcher))
         (new-descs (cl-loop for (dir . desc) in old-descs
                             if (string-prefix-p root dir)
                             do (file-notify-rm-watch desc)
                             else collect (cons dir desc))))
    (kele--fnr-update-descs watcher new-descs)))

(defun kele--fnr-update-renamed-directory (watcher old-name new-name)
  "Using the recursive WATCHER, update watching from OLD-NAME to NEW-NAME."
  (kele--fnr-update-stopped-directory watcher old-name)
  (kele--fnr-update-created-directory watcher new-name))

(defun kele--fnr-directory-watched-p (watcher directory)
  "Return t if DIRECTORY is watched by recursive WATCHER, else nil."
  (cl-loop for (dir . _desc) in (kele--fnr-watch-descs watcher)
           when (string= dir directory) return t))

(defun kele--fnr-directory-actionable-p (watcher directory)
  "Check whether WATCHER can react to DIRECTORY with an action."
  (if (file-directory-p directory)
      (not (string-match-p (kele--fnr-watch-regexp watcher)
                           (file-name-nondirectory directory)))
    ;; Directory might no longer exist, but can still be watched, in which case
    ;; it's still actionable.
    (kele--fnr-directory-watched-p watcher directory)))

(defun kele--fnr-clear-all ()
  "Clear all recursive filenotify watches."
  (interactive)
  (maphash (lambda (_uuid watcher)
             (kele--fnr-rm-watchers (kele--fnr-watch-descs watcher)))
           kele--fnr-descriptors)
  (setq kele--fnr-descriptors (make-hash-table :test 'equal)))

(provide 'kele-fnr)

;;; kele-fnr.el ends here
