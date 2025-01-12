;;; -*- lexical-binding: t -*-

(use-package xclip)

(use-package treesit-auto)

(use-package evil
  :config (evil-mode 1))

(use-package emacs
  :init (setq inhibit-startup-screen t
              backup-directory-alist `(("." . "~/.emacs_saves")))
  :config (global-display-line-numbers-mode))
