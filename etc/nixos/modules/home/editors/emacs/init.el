;;; -*- lexical-binding: t -*-

(use-package xclip
  :config (xclip-mode 1))

(use-package treesit-auto
  :defer 1
  :custom (treesit-auto-install 'prompt)
  :config (treesit-auto-add-to-auto-mode-alist 'all)
          (global-treesit-auto-mode))

(use-package evil
  :commands (evil-mode))

;;. UI

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;;.. Completions

;; See https://protesilaos.com/codelog/2024-02-17-emacs-modern-minibuffer-packages/
;; See https://www.youtube.com/watch?v=SOxlQ7ogplA

(use-package vertico
  :defer 1
  :config
  (setq vertico-cycle t
        vertico-resize nil)
  (vertico-mode 1))

(use-package marginalia
  :defer 1
  :config
  (marginalia-mode 1))

(use-package orderless
  :defer 1
  :config
  (setq completion-styles '(orderless basic)))

(use-package consult
  :bind (:map global-map
         ("M-s M-r" . consult-recent-file)
         ("M-s M-g" . consult-rg)
         ("M-s M-f" . consult-fd)
         ("M-s M-o" . consult-outline)
         ("M-s M-l" . consult-line)
         ("M-s M-b" . consult-buffer)))

(use-package embark
  :bind (:map global-map
         ("C-." . embark-act)
         :map minibuffer-local-map
         ("C-c C-c" . embark-collect)
         ("C-c C-e" . embark-export)))

(use-package embark-consult
  :after (embark consult))

(use-package corfu
  :defer 1
  :config
  (global-corfu-mode))

(unless (display-graphic-p)
  (use-package corfu-terminal
    :after corfu
    :config (corfu-terminal-mode 1)))

(use-package cape
  :after corfu
  ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
  ;; Press C-c p ? to for help.
  :bind ("C-c p" . cape-prefix-map) ;; Alternative key: M-<tab>, M-p, M-+
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

;;.. Fonts

(use-package nerd-icons
  :defer 1
  :config
  (when (display-graphic-p)
    (nerd-icons-set-font)))

(use-package ligature
  :defer 1
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

;;. Emacs config

(use-package emacs
  :custom
  (tab-always-indent 'complete)
  :config
  (setq inhibit-startup-screen t              ; Don't show startup message
        confirm-kill-emacs 'y-or-n-p          ; Less typing when quitting
        global-auto-revert-non-file-buffers t ; Revert dired
        auto-save-file-name-transforms `((".*" "~/.emacs.d/auto-saves/" t))
        backup-directory-alist `(("." . "~/.emacs.d/backups/")))
  (setq-default line-spacing 4)
  (set-frame-font "MonoLisa 12" nil t)
  (recentf-mode 1)
  (savehist-mode 1)
  (save-place-mode 1)
  (global-display-line-numbers-mode)
  (global-hl-line-mode 1)
  (global-visual-line-mode 1)
  (global-auto-revert-mode t)
  (winner-mode 1))
