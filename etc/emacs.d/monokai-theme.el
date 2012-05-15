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

(defvar monokai-colors
  '((((class color) (min-colors 65535))
     (mt-black . "#000000")
     (mt-sel-border . "#222218")
     (mt-bg . "#272822")
     (mt-fg . "#F8F8F2")
     (mt-invis . "#3B3A32")
     (mt-hi . "#3E3D32")
     (mt-sel . "#49483E")
     (mt-ag . "#9D550F")
     (mt-blue . "#66D9EF")
     (mt-mud . "#75715E")
     (mt-green . "#A6E22E")
     (mt-purple . "#AE81FF")
     (mt-beige . "#CFCFC2")
     (mt-gold . "#E6DB74")
     (mt-white . "#F8F8F0")
     (mt-red . "#F92672")
     (mt-orange . "#FD971F")
     (mt-yellow . "#FFE792"))
    (t
     ;; TODO(pope): Come up with term-friendly colors.
     (mt-black . "color-0")
     (mt-sel-border . "#222218")
     (mt-bg . "#272822")
     (mt-fg . "#F8F8F2")
     (mt-invis . "#3B3A32")
     (mt-hi . "#3E3D32")
     (mt-sel . "#49483E")
     (mt-ag . "#9D550F")
     (mt-blue . "#66D9EF")
     (mt-mud . "#75715E")
     (mt-green . "#A6E22E")
     (mt-purple . "#AE81FF")
     (mt-beige . "#CFCFC2")
     (mt-gold . "#E6DB74")
     (mt-white . "#F8F8F0")
     (mt-red . "#F92672")
     (mt-orange . "#FD971F")
     (mt-yellow . "#FFE792")))
  "The color values for each color name for a given
      condition. The format is: ((condition) (key . value) (key
      . value) ...)")

(defun monokai-expand-face (face)
  "Expands the simple face declaration with the color
  conditions."
  (let ((spec (car face))
        (props (cadr face)))
    (list spec (mapcar
                '(lambda (entry)
                   (let ((color-condition (car entry)))
                     (list color-condition
                           (monokai-expand-colors (cdr entry) props))))
                monokai-colors))))

(defun monokai-expand-colors (color-alist props)
  (let ((result '()))
    (while (car props)
      (let ((key (car props))
            (val (cadr props)))
        (if (memq key '(:foreground :background :color))
            (setq val (or (cdr (assq val color-alist)) val)))
        (if (listp val)
            (setq val (monokai-expand-colors entry val)))
        (setq result (append result `(,key ,val))))
      (setq props (cddr props)))
    result))

(defun monokai-theme-set-faces (theme &rest faces)
  (apply 'custom-theme-set-faces
         (append (list theme)
                 (mapcar 'monokai-expand-face faces))))

(monokai-theme-set-faces
 'monokai
 ;; Frame
 '(default (:foreground mt-fg :background mt-bg))
 '(cursor (:foreground mt-fg))
 '(highlight (:background mt-sel-border))
 '(minibuffer-prompt (:foreground mt-red))
 '(mode-line (:background mt-bg :foreground mt-white))
 '(mode-line-inactive (:background mt-hi :foreground mt-fg))
 '(region (:background mt-hi))
 '(fringe (:background mt-bg :foreground mt-invis))
 '(vertical-border (:foreground mt-black))

 ;; Main
 '(variable-pitch (:family "Arial"))
 '(show-paren-match-face (:foreground mt-fg :underline t :background mt-hi))
 '(escape-glyph (:foreground mt-invis))
 '(font-lock-builtin-face (:foreground mt-blue))
 '(font-lock-comment-face (:foreground mt-mud))
 '(font-lock-constant-face (:foreground mt-purple))
 '(font-lock-doc-string-face (:foreground mt-mud))
 '(font-lock-doc-face (:foreground mt-mud))
 '(font-lock-function-name-face (:foreground mt-green))
 '(font-lock-keyword-face (:foreground mt-red))
 '(font-lock-string-face (:foreground mt-yellow))
 '(font-lock-type-face (:foreground mt-blue))
 '(font-lock-variable-name-face (:foreground mt-blue))
 '(font-lock-warning-face (:bold t :foreground mt-red))

 ;; isearch
 '(isearch (:background mt-yellow :foreground mt-bg))
 '(isearch-fail (:background mt-orange :foreground mt-bg))
 '(lazy-highlight (:background mt-sel :foreground nil))

 ;; Minimap
 '(minimap-active-region-background (:background mt-hi))

 ;; js2-mode
 '(js2-jsdoc-tag (:italic t :foreground mt-mud))
 '(js2-jsdoc-type (:italic t :foreground mt-mud))
 '(js2-jsdoc-value (:bold t :italic t :foreground mt-mud))
 '(js2-function-param (:foreground mt-orange))

 ;; Dired
 '(dired-directory (:foreground mt-purple))
 '(dired-symlink (:foreground mt-blue))
 '(dired-mark (:foreground mt-green))
 '(dired-flagged (:foreground mt-orange))
 '(dired-marked (:foreground mt-yellow))
 '(dired-header (:foreground mt-fg :height 1.4 :underline t :inherit 'variable-pitch))

 ;; Org
 '(org-done (:foreground mt-green))
 '(org-todo (:foreground mt-red))
 '(org-document-info (:foreground mt-yellow))
 '(org-document-title (:foreground mt-fg :inherit 'variable-pitch :slant italic :height 2.0))
 '(outline-1 (:foreground mt-fg :inherit 'variable-pitch :slant italic :height 1.8))
 '(outline-2 (:foreground mt-fg :inherit 'variable-pitch :slant italic :height 1.6))
 '(outline-3 (:foreground mt-fg :inherit 'variable-pitch :slant italic :height 1.4))
 '(outline-4 (:foreground mt-fg :inherit 'variable-pitch :slant italic :height 1.2))
 '(outline-5 (:foreground mt-fg :inherit 'variable-pitch :slant italic :height 1.1))
 '(outline-6 (:foreground mt-fg :inherit 'variable-pitch :slant italic :height 1.1))
 '(outline-7 (:foreground mt-fg :inherit 'variable-pitch :slant italic :height 1.1))
 '(outline-8 (:foreground mt-fg :inherit 'variable-pitch :slant italic :height 1.1))

 ;; Magit
 '(magit-diff-add (:foreground mt-green))
 '(magit-diff-del (:foreground mt-red))
 '(magit-log-sha1 (:foreground mt-orange))

 ;; UI
 '(link (:foreground mt-blue))
 '(link-visited (:foreground mt-purple))
 '(button (:foreground mt-blue))

 ;; Ido
 '(ido-subdir (:foreground mt-purple))
 '(ido-only-match (:foreground mt-green))
 '(ido-virtual (:foreground mt-blue))

 ;; Whitespace
 '(whitespace-tab (:foreground mt-invis :background nil))
 '(whitespace-space (:foreground mt-invis :background nil))
 '(whitespace-newline (:foreground mt-invis :background nil))
 )

(provide-theme 'monokai)
