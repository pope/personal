;;; sync-mode.el ---  A minor mode for syncing files on save
;;
;; Copyright (C) 2011 K. Adam Christensen <pope@shifteleven.com>
;;
;; Licensed under the same terms as Emacs.
;;
;; Author: K. Adam Christensen <pope@shifteleven.com>
;; Maintainer: K. Adam Christensen <pope@shifteleven.com>
;; Created: 13 Feb 2011
;; Version: 0.1
;; Keywords: projects, sync
;;
;; This file is not a part of GNU Emacs.
;;
;;; Commentary:
;;
;; Upon save, syncs the buffer to another file on your local hard drive.
;;
;; Right now, it's pretty limited.  It requires that you set up a
;;location to sync from and a location to sync to and it requires that
;; both of those locations are within your `eproject-root'.  To set up
;; those variables, I'm currently using dir-locals to do so.
;;
;; Example in .dir-locals.el
;;
;; ((nil . ((mode . sync)
;;          (sync-to . "target/build/")
;;          (sync-from . "src/"))))
;;
;; Right now, it's very rough around the edges, but as I use this
;; more, I expect this to flush out a little better
;;
;; ** TODO Set a defcustom to have a list of user-defined syncing targets
;; ** TODO Allow the project root to be specified and not rely on eproject
;; ** TODO only use the project root if the syncing to and syncing from are relative paths
;; ** TODO see about using `tramp' to provide non-local syncing
;; ** TODO only sync a file if it's inside of the sync-from directory

(require 'eproject)

(defvar sync-from nil "Syncing source")
(defvar sync-to nil "Syncing destination")

(define-minor-mode sync-mode
  "A minor mode to sync a buffer from one directory to another."
  :lighter " sync"
  :group sync
  (if sync-mode
      (progn
        (add-hook 'after-save-hook 'sync nil t))
    (progn
      (remove-hook 'after-save-hook 'sync t))))

(defun turn-on-sync ()
  "Turn on sync-mode"
  (interactive)
  (sync-mode t))

(defun turn-off-sync ()
  "Turn off sync-mode"
  (interactive)
  (sync-mode nil))

(defun sync ()
  "Sync a buffer from one directory to another."
  (interactive)
  (if (and sync-from
           sync-to
           (string-prefix-p (eproject-root) (buffer-file-name)))
      (let ((sync-to-fullpath (concat (eproject-root)
                                      sync-to
                                      (substring (file-name-directory (buffer-file-name))
                                                 (length (concat (eproject-root)
                                                                 sync-from))))))
        (message "Syncing from %s to %s" (buffer-file-name) sync-to-fullpath)
        (mkdir sync-to-fullpath t)
        (copy-file (buffer-file-name)
                   sync-to-fullpath
                   t))
    (message "I don't know where to sync this file to!"))
  nil)

;; Mumamo Support

(eval-after-load "mumamo"
  '(progn
     (put 'sync-mode 'permanent-local-hook t)
     (put 'sync 'permanent-local-hook t)
     (put 'sync-mode 'permanent-local t)
     (put 'sync-to 'permanent-local t)
     (put 'sync-from 'permanent-local t)))

(provide 'sync-mode)

