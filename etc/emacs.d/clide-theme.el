;;; clide-theme.el --- REQUIRES EMACS 24: Clide Color Theme for Emacs.

;; Copyright (C) 2012 K. Adam Christensen.
;;
;; Author: K. Adam Christensen <pope@shifteleven.com>
;; URL: TBD
;; Version: 0.0.1
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, version 3 of the License.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.

(unless (>= 24 emacs-major-version)
  (error "clide-theme requires Emacs 24 or later."))

(deftheme clide
  "A light theme.")

(custom-theme-set-faces
 'clide
 ;; Frame
 '(default ((((class color) (min-colors 65535))
             (:foreground "black" :background "white"))
            (t
             (:foreground "color-232" :background "color-231"))))
 '(cursor ((t (:background "red1"))))
 '(highlight ((t (:background "WhiteSmoke"))))
 '(minibuffer-prompt ((t (:background "LightYellow1" :foreground nil))))
 '(mode-line ((t (:box (:line-width 1 :color "grey56" :style nil) :background "grey95"))))
 '(mode-line-buffer-id ((t (:slant italic :bold nil))))
 '(mode-line-inactive ((t (:foreground "grey56" :background "grey95"))))
 '(region ((t (:background "LightSkyBlue1" :foreground nil))))
 '(fringe ((t (:background "gainsboro"))))
 '(shadow ((t (:foreground "grey32"))))

 ;; Main
 '(variable-pitch ((t (:family "Arial"))))
 '(show-paren-match ((t (:inherit region :background nil :foreground nil))))
 '(show-paren-mismatch ((t (:foreground nil :background "red1"))))
 '(escape-glyph ((t (:foreground "red1"))))
 '(font-lock-builtin-face ((t (:bold t :foreground "SlateBlue1"))))
 '(font-lock-comment-delimiter-face ((t (::weight bold))))
 '(font-lock-comment-face ((t (:slant italic :foreground "grey56"))))
 '(font-lock-constant-face ((t (:weight bold :foreground "orange1"))))
 '(font-lock-doc-face ((t (:inherit font-lock-comment-face))))
 '(font-lock-function-name-face ((t (:weight bold :foreground "DodgerBlue4"))))
 '(font-lock-keyword-face ((t (:weight bold :foreground "DodgerBlue1"))))
 '(font-lock-negation-char-face ((t (:foreground "red1"))))
 '(font-lock-preprocessor-face ((t (:foreground "ForestGreen"))))
 '(font-lock-regexp-grouping-backslash ((t (:weight bold :foreground "LimeGreen"))))
 '(font-lock-regexp-grouping-construct ((t (:inherit font-lock-regexp-grouping-backslash))))
 '(font-lock-string-face ((((class color) (min-colors 65535))
                           (:foreground "green4" :background "mint cream"))
                          (t
                           (:foreground "green4"))))
 '(font-lock-type-face ((t (:underline t))))
 '(font-lock-variable-name-face ((t (:foreground "red1"))))
 '(font-lock-warning-face ((t (:background "#FFFFFF" :foreground "orange1" :inverse-video t))))

 ;; isearch
 '(isearch ((t (:foreground nil :background nil :inherit region))))
 '(isearch-fail ((t (:foreground "red1"))))
 '(lazy-highlight ((t (:background "LightYellow1"))))

 ;; js2-mode
 '(js2-jsdoc-tag ((t (:italic t :foreground "grey56" :bold t))))
 '(js2-jsdoc-html-tag-name ((t (:italic t :foreground "grey56" :bold t))))
 '(js2-jsdoc-html-tag-delimiter ((t (:italic t :foreground "grey56" :bold t))))
 '(js2-jsdoc-type ((t (:italic t :foreground "grey56" :bold t))))
 '(js2-jsdoc-value ((t (:bold t :italic t :foreground "grey56"))))
 '(js2-function-param ((t (:foreground "orange1"))))

 ;; dired
 '(dired-header ((((class color) (min-colors 65535))
                  (:inherit variable-pitch :height 1.6 :slant italic))
                 (t
                  (:weight bold))))

 ;; Org
 '(org-done ((t (:foreground "green4"))))
 '(org-hide ((t (:foreground "grey94"))))
 '(org-document-info ((t (:foreground "DodgerBlue4" :bold t))))
 '(org-document-title ((t (:foreground nil :inherit 'variable-pitch :slant italic :height 2.0))))
 '(outline-1 ((t (:foreground nil :inherit 'variable-pitch :slant italic :height 1.8))))
 '(outline-2 ((t (:foreground nil :inherit 'variable-pitch :slant italic :height 1.6))))
 '(outline-3 ((t (:foreground nil :inherit 'variable-pitch :slant italic :height 1.4))))
 '(outline-4 ((t (:foreground nil :inherit 'variable-pitch :slant italic :height 1.2))))
 '(outline-5 ((t (:foreground nil :inherit 'variable-pitch :slant italic :height 1.1))))
 '(outline-6 ((t (:foreground nil :inherit 'variable-pitch :slant italic :height 1.1))))
 '(outline-7 ((t (:foreground nil :inherit 'variable-pitch :slant italic :height 1.1))))
 '(outline-8 ((t (:foreground nil :inherit 'variable-pitch :slant italic :height 1.1))))

  ;; rst
 '(rst-level-1-face ((t (:foreground nil :inherit 'variable-pitch :slant italic :height 1.8))))
 '(rst-level-2-face ((t (:foreground nil :inherit 'variable-pitch :slant italic :height 1.6))))
 '(rst-level-3-face ((t (:foreground nil :inherit 'variable-pitch :slant italic :height 1.4))))
 '(rst-level-4-face ((t (:foreground nil :inherit 'variable-pitch :slant italic :height 1.2))))
 '(rst-level-5-face ((t (:foreground nil :inherit 'variable-pitch :slant italic :height 1.1))))
 '(rst-level-6-face ((t (:foreground nil :inherit 'variable-pitch :slant italic :height 1.1))))

 ;; diff
 '(diff-added ((t (:foreground "green4"))))
 '(diff-removed ((t (:foreground "red1"))))

  ;; Magit
 '(magit-branch ((t (:foreground "DodgerBlue1" :background "LightCyan"))))
 '(magit-log-graph ((t (:foreground nil :inherit shadow))))
 '(magit-log-head-label-local ((t (:inherit magit-branch :box nil :foreground nil :background nil))))
 '(magit-log-head-label-remote ((t (:inherit magit-branch :box nil :foreground "green4" :background nil))))
 '(magit-log-head-label-tags ((t (:inherit magit-branch :box nil :foreground "orange1" :background nil))))
 '(magit-log-tag-label ((t (:foreground "orange1"))))
 '(magit-log-sha1 ((t (:foreground nil :weight bold))))
 '(magit-section-title ((((class color) (min-colors 65535))
                         (:inherit variable-pitch :height 1.6 :slant italic))
                        (t
                         (:weight bold))))

 ;; UI
 '(link ((t (:underline t :foreground "DodgerBlue1"))))
 '(link-visited ((t (:inherit link :foreground "DodgerBlue4"))))
 '(button ((t (:underline t :foreground "DodgerBlue1"))))
 )


(provide-theme 'clide)
