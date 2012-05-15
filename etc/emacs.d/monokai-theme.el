;;; monokai-theme.el --- REQUIRES EMACS 24: Monokai Color Theme for Emacs.

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
  (error "monokai-theme requires Emacs 24 or later."))

(deftheme monokai
  "Monokai color theme")

(let ((mt-black "#000000")
      (mt-sel-border "#222218")
      (mt-bg "#272822")
      (mt-fg "#F8F8F2")
      (mt-invis "#3B3A32")
      (mt-hi "#3E3D32")
      (mt-sel "#49483E")
      (mt-ag "#9D550F")
      (mt-blue "#66D9EF")
      (mt-mud "#75715E")
      (mt-green "#A6E22E")
      (mt-purple "#AE81FF")
      (mt-beige "#CFCFC2")
      (mt-gold "#E6DB74")
      (mt-white "#F8F8F0")
      (mt-red "#F92672")
      (mt-orange "#FD971F")
      (mt-yellow "#FFE792"))
  (custom-theme-set-faces
   'monokai
   ;; Frame
   `(default ((t (:foreground ,mt-fg :background ,mt-bg))))
   `(cursor ((t (:foreground ,mt-fg))))
   `(highlight ((t (:background ,mt-sel-border))))
   `(minibuffer-prompt ((t (:foreground ,mt-red))))
   `(mode-line ((t (:background ,mt-bg :foreground ,mt-white))))
   `(mode-line-inactive ((t (:background ,mt-hi :foreground ,mt-fg))))
   `(region ((t (:background ,mt-hi))))
   `(fringe ((t (:background ,mt-bg :foreground ,mt-invis))))
   `(vertical-border ((t (:foreground ,mt-black))))

   ;; Main
   '(variable-pitch ((t (:family "Arial"))))
   `(show-paren-match-face ((t (:foreground ,mt-fg :underline t :background ,mt-hi))))
   `(escape-glyph ((t (:foreground ,mt-invis))))
   `(font-lock-builtin-face ((t (:foreground ,mt-blue))))
   `(font-lock-comment-face ((t (:foreground ,mt-mud))))
   `(font-lock-constant-face ((t (:foreground ,mt-purple))))
   `(font-lock-doc-string-face ((t (:foreground ,mt-mud))))
   `(font-lock-doc-face ((t (:foreground ,mt-mud))))
   `(font-lock-function-name-face ((t (:foreground ,mt-green))))
   `(font-lock-keyword-face ((t (:foreground ,mt-red))))
   `(font-lock-string-face ((t (:foreground ,mt-yellow))))
   `(font-lock-type-face ((t (:foreground ,mt-blue))))
   `(font-lock-variable-name-face ((t (:foreground ,mt-blue))))
   `(font-lock-warning-face ((t (:bold t :foreground ,mt-red))))

   ;; isearch
   `(isearch ((t (:background ,mt-yellow :foreground ,mt-bg))))
   `(isearch-fail ((t (:background ,mt-orange :foreground ,mt-bg))))
   `(lazy-highlight ((t (:background ,mt-sel :foreground nil))))

   ;; Minimap
   `(minimap-active-region-background ((t (:background ,mt-hi))))

   ;; js2-mode
   `(js2-jsdoc-tag ((t (:italic t :foreground ,mt-mud))))
   `(js2-jsdoc-type ((t (:italic t :foreground ,mt-mud))))
   `(js2-jsdoc-value ((t (:bold t :italic t :foreground ,mt-mud))))
   `(js2-function-param ((t (:foreground ,mt-orange))))

   ;; Dired
   `(dired-directory ((t (:foreground ,mt-purple))))
   `(dired-symlink ((t (:foreground ,mt-blue))))
   `(dired-mark ((t (:foreground ,mt-green))))
   `(dired-flagged ((t (:foreground ,mt-orange))))
   `(dired-marked ((t (:foreground ,mt-yellow))))
   `(dired-header ((t (:foreground ,mt-fg :height 1.4 :underline t :inherit 'variable-pitch))))

   ;; Org
   `(org-done ((t (:foreground ,mt-green))))
   `(org-todo ((t (:foreground ,mt-red))))
   `(org-document-info ((t (:foreground ,mt-yellow))))
   `(org-document-title ((t (:foreground ,mt-fg :inherit 'variable-pitch :slant italic :height 2.0))))
   `(outline-1 ((t (:foreground ,mt-fg :inherit 'variable-pitch :slant italic :height 1.8))))
   `(outline-2 ((t (:foreground ,mt-fg :inherit 'variable-pitch :slant italic :height 1.6))))
   `(outline-3 ((t (:foreground ,mt-fg :inherit 'variable-pitch :slant italic :height 1.4))))
   `(outline-4 ((t (:foreground ,mt-fg :inherit 'variable-pitch :slant italic :height 1.2))))
   `(outline-5 ((t (:foreground ,mt-fg :inherit 'variable-pitch :slant italic :height 1.1))))
   `(outline-6 ((t (:foreground ,mt-fg :inherit 'variable-pitch :slant italic :height 1.1))))
   `(outline-7 ((t (:foreground ,mt-fg :inherit 'variable-pitch :slant italic :height 1.1))))
   `(outline-8 ((t (:foreground ,mt-fg :inherit 'variable-pitch :slant italic :height 1.1))))

   ;; Magit
   `(magit-diff-add ((t (:foreground ,mt-green))))
   `(magit-diff-del ((t (:foreground ,mt-red))))
   `(magit-log-sha1 ((t (:foreground ,mt-orange))))

   ;; UI
   `(link ((t (:foreground ,mt-blue))))
   `(link-visited ((t (:foreground ,mt-purple))))
   `(button ((t (:foreground ,mt-blue))))

   ;; Ido
   `(ido-subdir ((t (:foreground ,mt-purple))))
   `(ido-only-match ((t (:foreground ,mt-green))))
   `(ido-virtual ((t (:foreground ,mt-blue))))

   ;; Whitespace
   `(whitespace-tab ((t (:foreground ,mt-invis :background nil))))
   `(whitespace-space ((t (:foreground ,mt-invis :background nil))))
   `(whitespace-newline ((t (:foreground ,mt-invis :background nil))))
   ))

(provide-theme 'monokai)
