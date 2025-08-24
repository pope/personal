;;; -*- lexical-binding: t -*-

;; Measured in bytes.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 8 1000 1000)
                  gc-cons-percentage 0.1)))

(use-package doom-themes
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
  (load-theme 'doom-nord t)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(defun pope-set-background-alpha (val)
  (interactive "nAlpha: ")
  (set-frame-parameter nil 'alpha-background val)
  (add-to-list 'default-frame-alist `(alpha-background . ,val)))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file t)

(use-package emacs
  :custom
  (recentf-mode t)
  (save-place-mode t)
  :init
  (setq auto-save-file-name-transforms `((".*" "~/.emacs.d/auto-saves/" t))
        backup-directory-alist         `(("." . "~/.emacs.d/backups/"))))

(use-package emacs
  :custom
  (fill-column 78)
  (editorconfig-mode t)
  (global-auto-revert-non-file-buffers t)
  (global-auto-revert-mode t))

(use-package expand-region
  :bind ("M-SPC" . er/expand-region))

(use-package multiple-cursors
  :bind
  ("C-S-c C-S-c"  . mc/edit-lines)
  ("C->"          . mc/mark-next-like-this)
  ("C-<"          . mc/mark-previous-like-this)
  ("C-c C-<"      . mc/mark-all-like-this))

(use-package xclip
  :custom (xclip-mode t))

(use-package clipetty
  :custom (global-clipetty-mode t))

(use-package evil
  :commands (evil-mode))

(use-package minibuffer
  :custom
  (completion-auto-help 'visible)
  (completion-auto-select t) ;; Show completion on first call
  (completion-show-help nil) ;; Skip docs for M-<down>, M-<up>, M-<RET>
  (completion-show-inline-help t)
  (completions-detailed t)
  (completions-max-height 20)
  (completions-sort 'historical)
  ;; Yo dawg, I heard you like minibuffers.
  (enable-recursive-minibuffers t))

(use-package vertico
  :custom
  (vertico-cycle t)
  (vertico-resize nil)
  (vertico-mode t))

;; Enable saving of minibuffer history
(use-package savehist
  :custom (history-delete-duplicates t)
  :hook (after-init . savehist-mode))

(use-package simple
  :custom
  ;; Hide commands in M-x that are incompatible for the current mode.
  (read-extended-command-predicate #'command-completion-default-include-p))

(use-package crm
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator))

;; Do not allow the cursor in the minibuffer prompt
(use-package emacs
  :custom
  (minibuffer-prompt-properties
   '(read-only-mode t cursor-intangible-mode t face minibuffer-prompt))
  :hook
  (minibuffer-setup-hook . cursor-intangible-mode))

(use-package marginalia
  :demand 1
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :custom (marginalia-mode t))

(use-package orderless
  :custom
  ;; These are minibuffer custom variables, but tailored for orderless
  (completion-styles '(orderless basic))
  (completion-category-overrides
   '((file (styles basic partial-completion)))))

(use-package consult
  :bind (:map global-map
              ("M-s M-r" . consult-recent-file)
              ("M-s M-g" . consult-ripgrep)
              ("M-s M-f" . consult-fd)
              ("M-s M-o" . consult-outline)
              ("M-s M-i" . consult-imenu)
              ("M-s M-l" . consult-line)
              ("M-s M-b" . consult-buffer)))

(use-package embark
  :bind (("C-."    . embark-act)       ;; pick some comfortable binding
         ("C-;"    . embark-dwim)      ;; good alternative: M-.
         ("C-h B"  . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package corfu
  :custom
  (corfu-cycle t)
  ;; Fixes a bug for me where the first item is wonky
  (corfu-preselect 'prompt)
  :init
  (global-corfu-mode t)
  (corfu-history-mode)
  (corfu-popupinfo-mode))

(use-package emacs
  :custom
  (tab-always-indent 'complete)
  (text-mode-ispell-word-completion nil)
  (read-extended-command-predicate #'command-completion-default-include-p))

(unless (display-graphic-p)
  (use-package corfu-terminal
    :after corfu
    :custom (corfu-terminal-mode t)))

(use-package emacs
  :custom
  (display-line-numbers-grow-only t)
  (inhibit-startup-screen t)     ; Don't show startup message
  (confirm-kill-emacs 'y-or-n-p) ; Less typing when quitting
  (column-number-mode t)
  (global-display-line-numbers-mode t)
  (global-hl-line-mode t)
  (global-visual-line-mode t)
  (tool-bar-mode nil)
  (winner-mode t))

(use-package diff-hl
  :custom
  (diff-hl-update-async t)
  (diff-hl-margin-mode t)
  :hook
  (after-init . global-diff-hl-mode))

(use-package nerd-icons
  :config
  (when (display-graphic-p)
    (nerd-icons-set-font)))

(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))
(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))
(use-package nerd-icons-corfu
  :after corfu
  :config (add-to-list 'corfu-margin-formatters
                       #'nerd-icons-corfu-formatter))
(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook
            #'nerd-icons-completion-marginalia-setup))

(use-package ligature
  :config
  ;; Enable all ligatures in programming modes
  (ligature-set-ligatures
   'prog-mode
   '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
     ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
     "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
     "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
     "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
     "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
     "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
     "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
     ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
     "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
     "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
     "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
     "\\\\" "://"))
  (global-ligature-mode t))

(use-package nyan-mode
  :custom (nyan-cat-face-number 4))

(use-package emacs
  :custom
  (mouse-wheel-tilt-scroll t)
  (scroll-conservatively 101)
  (scroll-margin 0)
  (scroll-bar-mode nil)
  (pixel-scroll-precision-mode t))

(unless (display-graphic-p)
  (use-package emacs
    :config (xterm-mouse-mode 1)))

(use-package indent-bars
  :custom
  (indent-bars-color '(highlight :face-bg t :blend 0.2))
  (indent-bars-highlight-current-depth '(:blend 0.5))
  (indent-bars-pad-frac 0.1)
  (indent-bars-pattern ".")
  (indent-bars-starting-column 0)
  (indent-bars-treesit-support t)
  (indent-bars-width-frac 0.1)
  (indent-bars-zigzag nil)
  :config (require 'indent-bars-ts)
  :hook (prog-mode . indent-bars-mode))

(use-package doom-modeline
  :custom
  (doom-modeline-minor-modes nil)
  :hook
  (after-init . doom-modeline-mode))

(setq major-mode-remap-alist
      '(
        (bash-mode        . bash-ts-mode)
        (c-mode           . c-ts-mode)
        (c++-mode         . c++-ts-mode)
        (c-or-c++-mode    . c-or-c++-ts-mode)
        (css-mode         . css-ts-mode)
        (js-mode          . js-ts-mode)
        (js2-mode         . js-ts-mode)
        (java-mode        . java-ts-mode)
        (json-mode        . json-ts-mode)
        (ruby-mode        . ruby-ts-mode)
        (nix-mode         . nix-ts-mode)
        (python-mode      . python-ts-mode)
        (typescript-mode  . typescript-ts-mode)
        (yaml-mode        . yaml-ts-mode)
        (zig-mode         . zig-ts-mode)))

(use-package go-ts-mode
  :mode "\\.go\\'")
(use-package rust-ts-mode
  :mode "\\.rs\\'")

(with-eval-after-load 'eglot
  (dolist (el '((nix-ts-mode . ("nixd"))
                (zig-ts-mode . ("zls"))))
    (add-to-list 'eglot-server-programs el)))

(use-package eglot
  :hook ((c-ts-mode     . eglot-ensure)
         (c++-ts-mode   . eglot-ensure)
         (go-ts-mode    . eglot-ensure)
         (nix-ts-mode   . eglot-ensure)
         (rust-ts-mode  . eglot-ensure)
         (zig-ts-mode   . eglot-ensure)))

(with-eval-after-load 'eglot
  (defun pope--eglot-ensure-formatting ()
    (if (eglot-managed-p)
        (add-hook 'before-save-hook #'eglot-format-buffer nil t)
      (remove-hook 'before-save-hook #'eglot-format-buffer t)))

  (add-hook 'eglot-managed-mode-hook #'pope--eglot-ensure-formatting))

(use-package direnv
  :custom (direnv-mode t))

(use-package vterm
  :commands (vterm)
  :config
  (add-hook 'vterm-mode-hook
            (lambda ()
              (display-line-numbers-mode -1)
              (visual-line-mode -1)
              (toggle-truncate-lines 1))))

(use-package emacs
  :custom-face
  (default (nil (:font "Monospace")))
  (fixed-pitch (nil (:font "Monospace")))
  (variable-pitch (nil (:family "Sans Serif") (:height 1.2))))

(defun pope-set-document-faces (&optional theme)
  (interactive)
  ;; Ensure line numbers have fixed pitchs so that left alignment
  ;; isn't wonky
  (set-face-attribute 'line-number nil :inherit 'fixed-pitch :height 0.9)
  (set-face-attribute 'line-number-current-line nil
                      :inherit 'fixed-pitch :height 0.9)

  ;; org-mode faces. If one face exists, set them all
  (when (facep 'org-default)
    (dolist (face '((org-level-1 . 1.35)
                    (org-level-2 . 1.3)
                    (org-level-3 . 1.2)
                    (org-level-4 . 1.1)
                    (org-level-5 . 1.1)
                    (org-level-6 . 1.1)
                    (org-level-7 . 1.1)
                    (org-level-8 . 1.1)))
      (set-face-attribute (car face) nil :weight 'bold :height (cdr face)))
    (set-face-attribute 'org-document-title nil :weight 'bold :height 1.8)
    (set-face-attribute 'org-block nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-special-keyword nil
                        :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-meta-line nil
                        :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

  ;; markdown-mode faces
  (when (facep 'markdown-header-face)
    (dolist (face '((markdown-header-face-1 . 1.35)
                    (markdown-header-face-2 . 1.3)
                    (markdown-header-face-3 . 1.2)
                    (markdown-header-face-4 . 1.1)
                    (markdown-header-face-5 . 1.1)
                    (markdown-header-face-6 . 1.1)))
      (set-face-attribute (car face) nil :weight 'bold :height (cdr face)))
    (set-face-attribute 'markdown-code-face nil :inherit 'fixed-pitch)))

(add-hook 'enable-theme-functions #'pope-set-document-faces)

(use-package org
  :custom
  (org-hide-leading-stars t)
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  :hook
  (org-mode . variable-pitch-mode)
  (org-mode . (lambda ()
                (olivetti-mode t)
                (org-modern-mode t)
                (indent-bars-mode -1)
                (diff-hl-margin-mode -1)))
  :config
  (pope-set-document-faces))

(use-package org-indent
  :config
  (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch)))

(use-package markdown-mode
  :custom
  (markdown-enable-highlighting-syntax t)
  (markdown-fontify-code-blocks-natively t)
  (markdown-hide-markup t)
  :hook
  (markdown-mode . (lambda ()
                     (variable-pitch-mode t)
                     (olivetti-mode t)
                     (indent-bars-mode -1)
                     (diff-hl-margin-mode -1)))
  :config
  (pope-set-document-faces))
