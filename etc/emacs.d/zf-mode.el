(require 'eproject)

;;;###autoload
(define-minor-mode zf-mode
  "A minor mode for when you're working with a ZendFramework project."
  :lighter " zf"
  :group zf
  :keymap
  `(
    (,(kbd "C-c z a")     . zf-goto-alt)
    (,(kbd "C-c z t")     . zf-goto-test)
    (,(kbd "C-c C-z t")   . zf-testfile-via-ant)
    (,(kbd "C-c C-z S-t") . zf-test-via-ant)
    ))

(defun turn-on-zf ()
  "Turn on zf-mode"
  (interactive)
  (if (not zf-mode) (zf-mode)))

(defun turn-off-zf ()
  "Turn off zf-mode"
  (interactive)
  (if zf-mode (zf-mode)))

;; COMMANDS

(defun zf-goto-test ()
  (interactive)
  (save-match-data
    (zf/goto-file
     (cond ((string-match "^\\(.*?\\)/tests/\\(application\\|library\\)/\\(.*?\\)Test\\.php$"
                          (buffer-file-name))
            (replace-match "\\1/\\2/\\3.php"
                           t nil (buffer-file-name)))
           ((string-match "^\\(.*?\\)/\\(application\\|library\\)/\\(.*?\\)\\.php$"
                          (buffer-file-name))
            (replace-match "\\1/tests/\\2/\\3Test.php"
                           t nil (buffer-file-name)))
           (t
            nil)))))

(defun zf-goto-alt ()
  (interactive)
  (save-match-data
    (cond ((string-match "^\\(.*?\\)/controllers/\\(.*\\)Controller\\.php$"
                         (buffer-file-name))
           (let ((bname (match-string 1 (buffer-file-name)))
                 (cname (zf/camel-to-hyphens
                         (match-string 2 (buffer-file-name))))
                 (sname (zf/camel-to-hyphens
                         (save-excursion
                           (save-restriction
                             (narrow-to-defun)
                             (goto-char (point-min))
                             (replace-regexp-in-string "^.*function \\(.*\\)Action.*\n"
                                                       "\\1"
                                                       (thing-at-point 'line) t nil))))))
             (zf/goto-file
              (concat bname "/views/scripts/" cname "/" sname ".phtml"))))
          ((string-match "^\\(.*?\\)/views/scripts/\\(.*?\\)/\\(.*?\\)\\.phtml$"
                         (buffer-file-name))
           (let ((bname (match-string 1 (buffer-file-name)))
                 (cname (zf/hyphens-to-title
                         (match-string 2 (buffer-file-name))))
                 (vname (concat (zf/hyphens-to-camel (match-string 3 (buffer-file-name)))
                                "Action")))
             (if (zf/goto-file
                  (concat bname "/controllers/" cname "Controller.php"))
                 (progn
                   (goto-char (point-min))
                   (re-search-forward vname)))))
          (t
           nil))))

(defun zf-testfile-via-ant ()
  (interactive)
  (let ((default-directory (eproject-root))
        (phpunit-cmd "ant phpunit-one -Dtest="))
    (save-match-data
      (cond ((string-match "Test\\.php$" (buffer-file-name))
             (compile (concat phpunit-cmd (buffer-file-name))))
            (t
             (message "There is not test file to run"))))))

(defun zf-test-via-ant ()
  (interactive)
  (let ((default-directory (eproject-root)))
    (compile "ant phpunit")))

;; HELPERS

(defun zf/goto-file (file)
  (if file
      (if (or (file-exists-p file)
              (y-or-n-p (concat "Could not find " file
                                ". Would you like me to open it anyway? ")))
          (progn
            (find-file file)
            t)
        nil)
    (progn
      (message "File does not appear to be a ZF file")
      nil)))


(defun zf/camel-to-hyphens (str)
  (let ((case-fold-search nil)
        (fp (substring str 0 1))
        (lp (substring str 1)))
    (concat (downcase fp)
            (downcase
             (replace-regexp-in-string "[A-Z]" "-\\&" lp)))))

(defun zf/hyphens-to-title (str)
  (zf/hyphens-to-camel (capitalize str)))

(defun zf/hyphens-to-camel (str)
  (let ((case-fold-search nil))
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (while (re-search-forward "-\\([a-z]\\)" nil t)
        (replace-match (concat (upcase (match-string 1)))
                       t))
      (buffer-string))))

(defmacro zf/with-case-insensitive-search (&rest body)
  (let ((cfs case-fold-search))
    (setq case-fold-search nil)
    `(let ((result (progn ,@body)))
       (setq case-fold-search cfs)
       result)))

(provide 'zf-mode)
