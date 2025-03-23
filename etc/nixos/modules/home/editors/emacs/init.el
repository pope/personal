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
  (load-theme 'doom-one t)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(defun pope-set-background-alpha (val)
  (interactive "nAlpha: ")
  (set-frame-parameter nil 'alpha-background val)
  (add-to-list 'default-frame-alist `(alpha-background . ,val)))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file t)

(setq auto-save-file-name-transforms `((".*" "~/.emacs.d/auto-saves/" t))
      backup-directory-alist `(("." . "~/.emacs.d/backups/")))
(recentf-mode 1)
(save-place-mode 1)

(setq global-auto-revert-non-file-buffers t)
(global-auto-revert-mode t)

(use-package emacs
  :custom (editorconfig-mode t))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package xclip
  :config (xclip-mode 1))

(use-package evil
  :commands (evil-mode))

(use-package vertico
  :custom
  (vertico-cycle t)
  (vertico-resize nil)
  :config
  (vertico-mode 1))

;; Enable saving of minibuffer history
(use-package savehist
  :hook (after-init . savehist-mode))

(use-package emacs
  :custom
  ;; Yo dawg, I heard you like minibuffers.
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x that are incompatible for the current mode.
  (read-extended-command-predicate #'command-completion-default-include-p)

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
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only-mode t cursor-intangible-mode t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

(use-package marginalia
  :demand 1
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :config
  (marginalia-mode 1))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package consult-
  :bind (:map global-map
         ("M-s M-r" . consult-recent-file)
         ("M-s M-g" . consult-rg)
         ("M-s M-f" . consult-fd)
         ("M-s M-o" . consult-outline)
         ("M-s M-l" . consult-line)
         ("M-s M-b" . consult-buffer)))

(use-package embark
  :bind (("C-." . embark-act)         ;; pick some comfortable binding
         ("C-;" . embark-dwim)        ;; good alternative: M-.
         ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
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
  :config
  (global-corfu-mode 1))

(use-package emacs
  :custom (tab-always-indent 'complete))

(unless (display-graphic-p)
  (use-package corfu-terminal
    :after corfu
    :config (corfu-terminal-mode 1)))

(use-package emacs
  :custom
  (display-line-numbers-grow-only t)
  (inhibit-startup-screen t)		; Don't show startup message
  (confirm-kill-emacs 'y-or-n-p)	; Less typing when quitting
  :config
  (column-number-mode 1)
  (global-display-line-numbers-mode 1)
  (global-hl-line-mode 1)
  (global-visual-line-mode 1)

  (tool-bar-mode -1)

  (winner-mode 1))

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
  :config
  (scroll-bar-mode -1)
  (pixel-scroll-precision-mode 1))

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
        (bash-mode . bash-ts-mode)
        (c-mode . c-ts-mode)
        (c++-mode . c++-ts-mode)
        (c-or-c++-mode . c-or-c++-ts-mode)
        (css-mode . css-ts-mode)
        (js-mode . js-ts-mode)
        (js2-mode . js-ts-mode)
        (java-mode . java-ts-mode)
        (json-mode . json-ts-mode)
        (ruby-mode . ruby-ts-mode)
        (nix-mode . nix-ts-mode)
        (python-mode . python-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (yaml-mode . yaml-ts-mode)
        (zig-mode . zig-ts-mode)))

(use-package go-ts-mode
  :mode "\\.go\\'")
(use-package rust-ts-mode
  :mode "\\.rs\\'")

(with-eval-after-load 'eglot
  (dolist (el '((nix-ts-mode . ("nixd"))
                (zig-ts-mode . ("zls"))))
    (add-to-list 'eglot-server-programs el)))

(use-package eglot
  :hook ((c-ts-mode . eglot-ensure)
         (go-ts-mode . eglot-ensure)
         (nix-ts-mode . eglot-ensure)
         (rust-ts-mode . eglot-ensure)
         (zig-ts-mode . eglot-ensure)))

(use-package direnv
  :config
  (direnv-mode))

(use-package vterm
  :commands (vterm)
  :config
  (add-hook 'vterm-mode-hook
            (lambda ()
              (display-line-numbers-mode -1)
              (visual-line-mode -1)
              (toggle-truncate-lines 1))))
