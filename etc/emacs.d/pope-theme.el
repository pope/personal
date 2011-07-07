(deftheme pope
  "A light theme.")

(custom-theme-set-variables
 'pope
 '(ibuffer-deletion-face (quote font-lock-warning-face))
 '(ibuffer-filter-group-name-face (quote org-level-2))
 '(ibuffer-marked-face (quote font-lock-warning-face))
 )

(custom-theme-set-faces
 'pope
 '(bold ((t (:weight bold))))
 '(bold-italic ((t (:weight bold :slant italic))))
 '(cursor ((t (:background "red1"))))
 '(default ((((class color) (min-colors 65535))
             (:foreground "black" :background "white"))
            (t
             (:foreground "color-232" :background "color-231"))))
 '(diff-added ((t (:foreground "green4"))))
 '(diff-changed ((t (:foreground "orange1"))))
 '(diff-file-header ((t (:underline t))))
 '(diff-function ((t (:foreground "orange1"))))
 '(diff-header ((((class color) (min-colors 65535))
                 (:slant italic))
                (t
                 (:weight bold))))
 '(diff-hunk-header ((t (:weight bold))))
 '(diff-removed ((t (:foreground "red1"))))
 '(dired-header ((t (:inherit (org-level-2)))))
 '(eshell-prompt ((t (:inherit (font-lock-constant-face)))))
 '(fixed-pitch ((t :family "DejaVu Sans Mono")))
 '(font-lock-builtin-face ((t (:weight bold :foreground "SlateBlue1"))))
 '(font-lock-comment-delimiter-face ((t (:weight bold :inherit (font-lock-comment-face)))))
 '(font-lock-comment-face ((t (:slant italic :foreground "grey56"))))
 '(font-lock-constant-face ((t (:weight bold :foreground "orange1"))))
 '(font-lock-doc-face ((t (:weight bold :inherit (font-lock-comment-face)))))
 '(font-lock-function-name-face ((t (:weight bold :foreground "DodgerBlue4"))))
 '(font-lock-keyword-face ((t (:weight bold :foreground "DodgerBlue1"))))
 '(font-lock-negation-char-face ((t (:foreground "red1"))))
 '(font-lock-preprocessor-face ((t (:foreground "ForestGreen" :inherit (font-lock-builtin-face)))))
 '(font-lock-regexp-grouping-backslash ((t (:foreground "LimeGreen" :inherit (bold)))))
 '(font-lock-regexp-grouping-construct ((t (:inherit (font-lock-regexp-grouping-backslash)))))
 '(font-lock-string-face ((t (:foreground "green4"))))
 '(font-lock-type-face ((t (:underline t))))
 '(font-lock-variable-name-face ((t (:foreground "red1"))))
 '(font-lock-warning-face ((t (:background "#FFFFFF" :foreground "orange1" :inverse-video t))))
 '(fringe ((t (:background "gainsboro"))))
 '(header-line ((t (:inherit (mode-line)))))
 '(highlight ((t (:slant italic :underline "DodgerBlue1"))))
 '(hl-line ((t (:background "grey98"))))
 '(info-menu-header ((t (:inherit (org-level-5)))))
 '(info-title-1 ((t (:inherit (org-level-1)))))
 '(info-title-2 ((t (:inherit (org-level-2)))))
 '(info-title-3 ((t (:inherit (org-level-3)))))
 '(info-title-4 ((t (:inherit (org-level-4)))))
 '(isearch ((t (:inherit (region)))))
 '(isearch-fail ((t (:foreground "red1"))))
 '(italic ((t (:slant italic))))
 '(lazy-highlight ((t (:background "LightYellow1"))))
 '(link ((t (:underline t :foreground "DodgerBlue1"))))
 '(link-visited ((t (:foreground "DodgerBlue4" :inherit (link)))))
 '(magit-branch ((t (:foreground "DodgerBlue1" :background "LightCyan"))))
 '(magit-diff-add ((t (:inherit (diff-added)))))
 '(magit-diff-del ((t (:inherit (diff-removed)))))
 '(magit-diff-file-header ((t (:inherit (diff-file-header)))))
 '(magit-diff-hunk-header ((t (:inherit (diff-hunk-header)))))
 '(magit-diff-none ((t (:inherit (diff-context)))))
 '(magit-item-highlight ((t (:background "grey94"))))
 '(magit-item-mark ((t (:foreground "orange1"))))
 '(magit-log-graph ((t (:inherit (shadow)))))
 '(magit-log-head-label-local ((t (:inherit (magit-branch)))))
 '(magit-log-head-label-remote ((t (:foreground "green4" :inherit (magit-branch)))))
 '(magit-log-head-label-tags ((t (:inherit (magit-log-tag-label)))))
 '(magit-log-sha1 ((t (:weight bold))))
 '(magit-log-tag-label ((t (:foreground "orange1" :inherit (magit-branch)))))
 '(magit-section-title ((t (:inherit (org-level-2)))))
 '(match ((t (:background "LightYellow1"))))
 '(minibuffer-prompt ((t (:background "LightYellow1"))))
 '(mode-line ((t (:box (:line-width 1 :color "grey56" :style nil) :background "grey95"))))
 '(mode-line-buffer-id ((t (:slant italic))))
 '(mode-line-inactive ((t (:foreground "grey56" :background "grey95"))))
 '(mouse ((t (:foreground "#000000" :background "#FFFFFF"))))
 '(org-block ((t (:foreground "green4"))))
 '(org-code ((t (:foreground "green4" :inherit (shadow)))))
 '(org-done ((t (:foreground "green4"))))
 '(org-hide ((t (:foreground "grey94"))))
 '(org-level-1 ((((class color) (min-colors 65535))
                 (:height 1.8 :slant italic :inherit (variable-pitch)))
                (t
                 (:weight bold))))
 '(org-level-2 ((((class color) (min-colors 65535))
                 (:height 1.6 :slant italic :inherit (variable-pitch)))
                (t
                 (:weight bold))))
 '(org-level-3 ((((class color) (min-colors 65535))
                 (:height 1.4 :slant italic :inherit (variable-pitch)))
                (t
                 (:weight bold))))
 '(org-level-4 ((((class color) (min-colors 65535))
                 (:height 1.2 :slant italic :inherit (variable-pitch)))
                (t
                 (:weight bold))))
 '(org-level-5 ((((class color) (min-colors 65535))
                 (:height 1.2 :slant italic :inherit (variable-pitch)))
                (t
                 (:weight bold))))
 '(org-level-6 ((((class color) (min-colors 65535))
                 (:height 1.2 :slant italic :inherit (variable-pitch)))
                (t
                 (:weight bold))))
 '(org-level-7 ((((class color) (min-colors 65535))
                 (:height 1.2 :slant italic :inherit (variable-pitch)))
                (t
                 (:weight bold))))
 '(org-level-8 ((((class color) (min-colors 65535))
                 (:height 1.2 :slant italic :inherit (variable-pitch)))
                (t
                 (:weight bold))))
 '(org-meta-line ((t (:foreground "grey56" :inherit (font-lock-comment-face)))))
 '(org-table ((t (:foreground "DodgerBlue4"))))
 '(org-tag ((t (:height 0.8 :weight bold :foreground "CadetBlue4"))))
 '(org-todo ((t (:foreground "red1"))))
 '(region ((t (:background "LightSkyBlue1"))))
 '(shadow ((t (:foreground "grey32"))))
 '(show-paren-match ((t (:inherit (region)))))
 '(show-paren-mismatch ((t (:background "red1"))))
 '(variable-pitch ((t (:family "Arial"))))
 )

(provide-theme 'pope)
