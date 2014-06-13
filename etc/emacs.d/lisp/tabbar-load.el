;; My Setup for Tabbar.  This is in an autoload file because Aquamacs
;; doesn't use this

(eval-when-compile (require 'eproject))

(defun pope-setup-tabbar ()
  (require 'tabbar)
  (set-face-attribute 'tabbar-default-face nil
                      :background "gray80"
                      :foreground "black"
                      :height 0.8)
  (set-face-attribute 'tabbar-selected-face nil
                      :foreground "black"
                      :background "grey95"
                      :box '(:line-width 4 :color "grey95"))
  (set-face-attribute 'tabbar-separator-face nil
                      :background "grey50"
                      :foreground "black"
                      :height 0.2)
  (set-face-attribute 'tabbar-unselected-face nil
                      :foreground "black"
                      :box '(:line-width 4 :color "grey80"))
  (set-face-attribute 'tabbar-button-face nil
                      :foreground "grey35"
                      :box '(:line-width 2 :color "grey80")
                      :height 1.2)
  (setq tabbar-separator '(" "))
  (tabbar-setup-button 'tabbar-home-button
                       (cons (cons "⏏" nil) (cons "⌂" nil)))
  (tabbar-setup-button 'tabbar-scroll-left-button
                       (cons (cons " ◀" nil) (cons " ◁" nil)))
  (tabbar-setup-button 'tabbar-scroll-right-button
                       (cons (cons " ▶ " nil) (cons " ▷ " nil)))
  (setq tabbar-buffer-groups-function 'pope-tabbar-buffer-groups)

  (tabbar-mode 1)

  (global-set-key (kbd "s-{") 'tabbar-backward)
  (global-set-key (kbd "s-}") 'tabbar-forward)
  (global-set-key (kbd "s-[") 'tabbar-forward-group)
  (global-set-key (kbd "s-]") 'tabbar-backward-group))


(defun pope-tabbar-buffer-groups (buffer)
  "Return the list of group names BUFFER belongs to.
Return only one group for each buffer."
  (with-current-buffer (get-buffer buffer)
    (cond ((or (get-buffer-process (current-buffer))
               (memq major-mode '(comint-mode compilation-mode)))
           '("Process"))
          ((member (buffer-name) '("*scratch*" "*Messages*")) '("Common"))
          ((eq major-mode 'dired-mode) '("Dired"))
          ((memq major-mode '(help-mode apropos-mode Info-mode Man-mode)) '("Help"))
          ((memq major-mode
                 '(rmail-mode
                   rmail-edit-mode vm-summary-mode vm-mode mail-mode
                   mh-letter-mode mh-show-mode mh-folder-mode
                   gnus-summary-mode message-mode gnus-group-mode
                   gnus-article-mode score-mode gnus-browse-killed-mode))
           '("Mail"))
          ((ignore-errors (eproject-root))
           (list
            ;; store it in the eproject group
            (file-name-nondirectory (directory-file-name (eproject-root)))
            ;; store it with other major modes
            (if (and (stringp mode-name) (string-match "[^ ]" mode-name))
                mode-name (symbol-name major-mode))))
          (t (list
              (if (and (stringp mode-name) (string-match "[^ ]" mode-name))
                  mode-name
                (symbol-name major-mode)))))))
