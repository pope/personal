(deftheme solarized-dark
  "Solarized theme - dark.")

(put 'solarized-dark 'theme-immediate t)

(setq solarized-colors
      '((((class color) (min-colors 65535))
         (base03  . "#002b36")
         (base02  . "#073642")
         (base01  . "#586e75")
         (base00  . "#657b83")
         (base0   . "#839496")
         (base1   . "#93a1a1")
         (base2   . "#eee8d5")
         (base3   . "#fdf6e3")
         (yellow  . "#b58900")
         (orange  . "#cb4b16")
         (red     . "#dc322f")
         (magenta . "#d33682")
         (violet  . "#6c71c4")
         (blue    . "#268bd2")
         (cyan    . "#2aa198")
         (green   . "#859900")
         )
        (t
         (base03  . "brightblack")
         (base02  . "black")
         (base01  . "brightgreen")
         (base00  . "brightyellow")
         (base0   . "brightblue")
         (base1   . "brightcyan")
         (base2   . "white")
         (base3   . "brightwhite")
         (yellow  . "yellow")
         (orange  . "brightred")
         (red     . "red")
         (magenta . "megenta")
         (violet  . "brightmagenta")
         (blue    . "blue")
         (cyan    . "cyan")
         (green   . "green")
         )))

(defun solarized-simple-face-to-multiple (face)
  (let ((spec (car face))
        (lst (cadr face)))
    (list spec (mapcar
                '(lambda (entry)
                   (let ((color-condition (car entry)))
                     (list color-condition (solarized-color-list-expand (cdr entry) lst))))
                solarized-colors))))

(defun solarized-color-list-expand (color-alist lst)
  (let ((result '()))
    (while (car lst)
      (let ((key (car lst))
            (val (cadr lst)))
        (if (memq key '(:foreground :background :color))
            (setq val (cdr (assq val color-alist))))
        (if (listp val)
            (setq val (solarized-color-list-expand entry val)))
        (setq result (append result `(,key ,val))))
      (setq lst (cddr lst)))
    result))

(defun solarized-theme-set-faces (theme &rest args)
  (apply 'custom-theme-set-faces (append (list theme) (mapcar 'solarized-simple-face-to-multiple args))))

(solarized-theme-set-faces
 'solarized-dark
  ;; basic
 '(default (:background base03 :foreground base0))
 '(cursor (:foreground base0 :background base03 :inverse-video t))
 '(escape-glyph-face (:foreground red))
 '(fringe (:foreground base01 :background base02))
 '(header-line (:foreground base0 :background base2))
 '(highlight (:background base02))
 '(hl-line (:background base02))
 '(isearch (:foreground yellow :inverse-video t))
 '(menu (:foreground base0 :background base02))
 '(minibuffer-prompt (:foreground blue))
 '(mode-line (:foreground base1 :background base02
                          :box (:line-width 1 :color base1)))
 '(mode-line-buffer-id (:foreground base1))
 '(mode-line-inactive (:foreground base0  :background base03
                                   :box (:line-width 1 :color base02)))
 '(region (:background base02))
 '(secondary-selection (:background base02))
 '(trailing-whitespace (:foreground red :inverse-video t))
 '(vertical-border (:foreground base0))

 ;; compilation
 '(compilation-info (:foreground green :bold t))
 '(compilation-warning (:foreground orange :bold t))

 ;; customize
 '(custom-button (:background base02 :box (:line-width 2 :style released-button)))
 '(custom-button-mouse (:inherit custom-button :foreground base1))
 '(custom-button-pressed (:inherit custom-button-mouse
                                   :box (:line-width 2 :style pressed-button)))
 '(custom-comment-tag (:background base02))
 '(custom-comment-tag (:background base02))
 '(custom-documentation (:inherit default))
 '(custom-group-tag (:foreground orange :bold t))
 '(custom-link (:foreground violet))
 '(custom-state (:foreground green))
 '(custom-variable-tag (:foreground orange :bold t))

 ;; diff
 '(diff-added (:foreground green :inverse-video t))
 '(diff-changed (:foreground yellow :inverse-video t))
 '(diff-removed (:foreground red :inverse-video t))

 ;; emacs-wiki
 '(emacs-wiki-bad-link-face (:foreground red :underline t))
 '(emacs-wiki-link-face (:foreground blue :underline t))
 '(emacs-wiki-verbatim-face (:foreground base00 :underline t))
 
 ;; font-lock
 '(font-lock-builtin-face (:foreground green))
 '(font-lock-comment-face (:foreground base01 :italic t))
 '(font-lock-constant-face (:foreground cyan))
 '(font-lock-function-name-face (:foreground blue))
 '(font-lock-keyword-face (:foreground green))
 '(font-lock-string-face (:foreground cyan))
 '(font-lock-type-face (:foreground yellow))
 '(font-lock-variable-name-face (:foreground blue))
 '(font-lock-warning-face (:foreground red :bold t))

 ;; info
 '(info-xref (:foreground blue :underline t))
 '(info-xref-visited (:inherit info-xref :foreground magenta))

 ;; org
 '(org-hide (:foreground base03))
 '(org-todo (:foreground red :bold t))
 '(org-done (:foreground green :bold t))

 ;; show-paren
 '(show-paren-match-face (:background cyan :foreground base3))
 '(show-paren-mismatch-face (:background red :foreground base3))
)

(provide-theme 'solarized-dark)
