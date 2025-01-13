;;; -*- lexical-binding: t -*-

(use-package xclip
  :config (xclip-mode 1))

(use-package treesit-auto
  :defer t
  :custom (treesit-auto-install 'prompt)
  :config (treesit-auto-add-to-auto-mode-alist 'all)
          (global-treesit-auto-mode))

(use-package evil
  :defer t
  :config (evil-mode 0))

;;. UI

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-tokyo-night t)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;;.. Fonts

(use-package nerd-icons
  :after prog-mode
  :config
  (nerd-icons-set-font))

(use-package ligature
  :after prog-mode
  :config
  ;; Enable all ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
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
  :config
  (setq inhibit-startup-screen t          ; Don't show startup message
	confirm-kill-emacs 'y-or-n-p      ; y and n instead of yes and no when quitting
	auto-save-file-name-transforms `((".*" "~/.emacs.d/auto-saves/" t))
        backup-directory-alist `(("." . "~/.emacs.d/backups/")))
  (setq-default line-spacing 4)
  (global-display-line-numbers-mode)
  (global-hl-line-mode 1)  (global-visual-line-mode 1)
  (global-auto-revert-mode t))
