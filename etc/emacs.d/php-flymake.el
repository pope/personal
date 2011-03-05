;; http://sachachua.com/blog/2008/07/emacs-and-php-on-the-fly-syntax-checking-with-flymake/
;; http://blog.arithm.com/2010/03/13/getting-flymake-to-work-with-emacs-nxhtml

;;;###autoload
(defun php-flymake ()
  "Use php to check the syntax of the current file."
  (let* ((temp (flymake-init-create-temp-buffer-copy 'flymake-create-temp-inplace))
	 (local (file-relative-name temp (file-name-directory buffer-file-name))))
    (list "php" (list "-f" local "-l"))))

(defvar *php-file-regex*
  "\\.\\(php[s34]?\\|phtml\\|inc\\|module\\|install\\|engine\\)")

;;;###autoload
(defun php/flymake-enable ()
  (if (string-match-p *php-file-regex* (buffer-file-name))
      (progn
        (require 'flymake)
        (flymake-mode 1))))

(eval-after-load 'flymake
  `(progn
     (add-to-list 'flymake-err-line-patterns
                  '("\\(Parse\\|Fatal\\) error: +\\(.*?\\) in \\(.*?\\) on line \\([0-9]+\\)$" 3 4 nil 2))
     (add-to-list 'flymake-allowed-file-name-masks '(,*php-file-regex* php-flymake))
     (eval-after-load 'php-mode
       '(progn
          (define-key php-mode-map '[M-S-up] 'flymake-goto-prev-error)
          (define-key php-mode-map '[M-S-down] 'flymake-goto-next-error)))))

(add-hook 'find-file-hook 'php/flymake-enable)

(provide 'php-flymake)
