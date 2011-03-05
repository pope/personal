;; A lot of this is stolen from emacs-starter-kit.  I'm pulling out
;; the bits I like!

(defvar *emacs-load-start* (float-time))


;;
;; The Basics
;;

(if (and
     (fboundp 'menu-bar-mode)
     (not (or (featurep 'aquamacs) ;; Aquamacs
	      (featurep 'ns))))    ;; Emacs.app
    (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq dotfiles-dir (file-name-directory
		    (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path dotfiles-dir)

(setq autoload-file (concat dotfiles-dir "loaddefs.el"))
(setq custom-file (concat dotfiles-dir "custom.el"))

(line-number-mode t)
(column-number-mode t)
(delete-selection-mode t)
(global-auto-revert-mode)
(global-hl-line-mode)
(set-face-attribute 'hl-line nil
		    :background "honeydew1")

(winner-mode)

;; Prefer UTF-8 goodness
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(setq visible-bell t
      ring-bell-function 'ignore
      inhibit-startup-message t
      mouse-yank-at-point t
      truncate-partial-width-windows nil
      debug-on-error t)

;; Don't clutter up directories with files~
(setq backup-directory-alist `(("." . ,(expand-file-name
					(concat dotfiles-dir "backups")))))

(load custom-file 'noerror)

;; load in a host specific file if it's there
(setq system-specific-config (concat dotfiles-dir system-name ".el"))
(if (file-exists-p system-specific-config)
    (progn
      (load system-specific-config)
      (message "init.el: %s loaded after %.1fs" (concat system-name ".el") (- (float-time) *emacs-load-start*))))


;;
;; el-get
;;

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(require 'el-get)
(require 'inversion) ;; needed by cedet

(setq package-user-dir "~/.emacs.d/elpa")

(setq el-get-sources
      '(el-get
	(:name google-maps :features ())
	(:name paredit :features ())
	naquadah-theme
	nxhtml
	auto-complete
	auto-complete-etags
	(:name zencoding-mode :features ())
	switch-window
	(:name buffer-move
	       :type emacswiki
	       :after (lambda ()
			(global-set-key (kbd "<C-S-up>")     'buf-move-up)
			(global-set-key (kbd "<C-S-down>")   'buf-move-down)
			(global-set-key (kbd "<C-S-left>")   'buf-move-left)
			(global-set-key (kbd "<C-S-right>")  'buf-move-right)))
	java-mode-indent-annotations
	(:name vkill
	       :features ()
	       :after (lambda ()
			(autoload 'vkill "vkill" nil t)
			(autoload 'list-unix-processes "vkill" nil t)))
	(:name rainbow-mode :features ())
	(:name google-weather :features ())
	(:name magit :features ())
	;;yasnippet
	org-mode
	(:name org-contacts :features ())
	(:name cedet
	       :type http-tar
	       :options ("xzf")
	       :url "http://shifteleven.com/mirrors/cedet-1.0.tar.gz"
	       :build `(,(concat "make EAMCS=" el-get-emacs))
	       :load-path ("./common")
	       :after (lambda ()
			(load-file "~/.emacs.d/el-get/cedet/common/cedet.el")))
	(:name ecb
	       :type http-tar
	       :options ("xzf")
	       :url "http://shifteleven.com/mirrors/ecb-2.40.tar.gz"
	       :build `(,(concat "make CEDET=~/.emacs.d/el-get/cedet EMACS=" el-get-emacs))
	       :after (lambda ()
			(setq ecb-primary-secondary-mouse-buttons 'mouse-1--mouse-2)))
	(:name textmate
	       :type git
	       :features textmate
	       :url "https://github.com/defunkt/textmate.el.git"
	       :after (lambda ()
			(textmate-mode)
			(add-to-list '*textmate-project-roots* "pom.xml")
			(setq *textmate-gf-exclude* (concat *textmate-gf-exclude* "|target"))))
	(:name gnus
	       :type git
	       :url "http://git.gnus.org/gnus.git"
	       :features (gnus-load)
	       :build `(,(concat "./configure --with-emacs=" el-get-emacs) "make")
	       :load-path ("./lisp"))
	(:name offlineimap :features ())
	(:name eproject
	       :type git
	       :features (eproject eproject-extras)
	       :url "https://github.com/jrockway/eproject.git")
	emacs-w3m
	(:name geben
	       :type svn
	       :url "http://geben-on-emacs.googlecode.com/svn/trunk/"
	       :build `(,(concat "make EMACS=" el-get-emacs)))))
(el-get)
(message "init.el: el-get loaded after %.1fs" (- (float-time) *emacs-load-start*))


;;
;; Defuns
;;

(require 'thingatpt)
(require 'imenu)

(defun ido-imenu ()
  "Update the imenu index and then use ido to select a symbol to navigate to.
Symbols matching the text at point are put first in the completion list."
  (interactive)
  (imenu--make-index-alist)
  (let ((name-and-pos '())
	(symbol-names '()))
    (let ((addsymbols (symbol-list)
		      (when (listp symbol-list)
			(dolist (symbol symbol-list)
			  (let ((name nil) (position nil))
			    (cond
			     ((and (listp symbol) (imenu--subalist-p symbol))
			      (addsymbols symbol))

			     ((listp symbol)
			      (setq name (car symbol))
			      (setq position (cdr symbol)))

			     ((stringp symbol)
			      (setq name symbol)
			      (setq position (get-text-property 1 'org-imenu-marker symbol))))

			    (unless (or (null position) (null name))
			      (add-to-list 'symbol-names name)
			      (add-to-list 'name-and-pos (cons name position))))))))
      (addsymbols imenu--index-alist))
    ;; If there are matching symbols at point, put them at the beginning of `symbol-names'.
    (let ((symbol-at-point (thing-at-point 'symbol)))
      (when symbol-at-point
	(let* ((regexp (concat (regexp-quote symbol-at-point) "$"))
	       (matching-symbols (delq nil (mapcar (lambda (symbol)
						     (if (string-match regexp symbol) symbol))
						   symbol-names))))
	  (when matching-symbols
	    (sort matching-symbols (lambda (a b) (> (length a) (length b))))
	    (mapc (lambda (symbol) (setq symbol-names (cons symbol (delete symbol symbol-names))))
		  matching-symbols)))))
    (let* ((selected-symbol (ido-completing-read "Symbol? " symbol-names))
	   (position (cdr (assoc selected-symbol name-and-pos))))
      (goto-char position))))

(defun recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

(defun sudo-edit (&optional arg)
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
	(filename (buffer-file-name)))
    (if (not filename)
	(message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
	  (message "A buffer named '%s' already exists!" new-name)
	(progn
	  (rename-file name new-name 1)
	  (rename-buffer new-name)
	  (set-visited-file-name new-name)
	  (set-buffer-modified-p nil))))))

(defun turn-on-paredit ()
  (paredit-mode t))

(defun turn-on-idle-highlight ()
  (idle-highlight t))

(defun turn-on-hl-line-mode ()
  (when (> (display-color-cells) 8) (hl-line-mode t)))

(defun add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\|REFACTOR\\):"
          1 font-lock-warning-face t))))

(defun pretty-lambdas ()
  (font-lock-add-keywords
   nil `(("(?\\(lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))


;;
;; Coding Basics
;;

;; TODO: Add the coding hooks


;;
;; Eshell
;;

(setq eshell-cmpl-cycle-completions nil
      eshell-save-history-on-exit t
      eshell-hist-ignoredups t
      eshell-history-size 512
      eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\|\\.svn\\|\\.git\\)/\\'")

(eval-after-load 'esh-opt
  '(progn
     (require 'em-prompt)
     (require 'em-term)
     (require 'em-cmpl)
     (setenv "PAGER" "cat")
     (set-face-attribute 'eshell-prompt nil :foreground "turquoise1")
     (add-hook 'eshell-mode-hook ;; for some reason this needs to be a hook
	       '(lambda () (eshell/export "TERM" "dumb")))
     (when (< emacs-major-version 23)
       (add-hook 'eshell-mode-hook ;; for some reason this needs to be a hook
		 '(lambda () (define-key eshell-mode-map "\C-a" 'eshell-bol)))
       (add-to-list 'eshell-output-filter-functions 'eshell-handle-ansi-color))

     ;; TODO: submit these via M-x report-emacs-bug
     (add-to-list 'eshell-visual-commands "ssh")
     (add-to-list 'eshell-visual-commands "tail")
     (add-to-list 'eshell-command-completions-alist
		  '("gunzip" "gz\\'"))
     (add-to-list 'eshell-command-completions-alist
		  '("tar" "\\(\\.tar|\\.tgz\\|\\.tar\\.gz\\)\\'"))))

(defun eshell/find (dir &rest opts)
  (find-dired dir (mapconcat 'identity opts " ")))

(defun eshell/clear ()
  "04Dec2001 - sailor, to clear the eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))


;;
;; Lisp
;;

(define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol)
(define-key lisp-mode-shared-map (kbd "C-c l") "lambda")
(define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)
(define-key lisp-mode-shared-map (kbd "C-\\") 'lisp-complete-symbol)
(define-key lisp-mode-shared-map (kbd "C-c v") 'eval-buffer)

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'esk-remove-elc-on-save)

(defun esk-remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))))

(dolist (x '(scheme emacs-lisp lisp clojure))
  (add-hook
   (intern (concat (symbol-name x) "-mode-hook")) 'turn-on-paredit)
  (add-hook
   (intern (concat (symbol-name x) "-mode-hook")) 'pretty-lambdas))


;;
;; PHP
;;

(defun php-mode-settings ()
  (setq c-basic-offset 4) ; 4 tabs indenting
  (setq indent-tabs-mode nil)
  (setq fill-column 78)
  (setq case-fold-search t)
  (c-set-offset 'case-label '+)
  (c-set-offset 'arglist-close 'c-lineup-arglist-operators)
  (c-set-offset 'arglist-intro '+) ; for FAPI arrays and DBTNG
  (c-set-offset 'arglist-cont-nonempty 'c-lineup-math)
  (setq php-mode-force-pear t)) ; for DBTNG fields and values

(eval-after-load "php-mode"
  '(progn
     (require 'php-flymake)
     (require 'zf-mode)
     (add-hook 'php-mode-hook 'turn-on-zf)))

(eval-after-load "geben"
  (setenv "XDEBUG_CONFIG" "idekey=geben_session"))


;;
;; Key Bindings
;;

;; You know, like Readline.
(global-set-key (kbd "C-M-h") 'backward-kill-word)

;; Align your code in a pretty way.
(global-set-key (kbd "C-x \\") 'align-regexp)

;; Completion that uses many different methods to find options.
(global-set-key (kbd "M-/") 'hippie-expand)

;; Turn on the menu bar for exploring new modes
(global-set-key (kbd "C-<f10>") 'menu-bar-mode)

;; Font size
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

;; Use regex searches by default.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "\C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; Jump to a definition in the current file. (This is awesome.)
(global-set-key (kbd "C-x C-i") 'ido-imenu)

;; File finding
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
(global-set-key (kbd "C-x f") 'recentf-ido-find-file)
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "M-`") 'file-cache-minibuffer-complete)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Start eshell or switch to it if it's active.
(global-set-key (kbd "C-x m") 'eshell)

;; Start a new eshell even if one is active.
(global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t)))

;; If you want to be able to M-x without meta (phones, etc)
(global-set-key (kbd "C-x C-m") 'execute-extended-command)

;; Help should search more than just commands
(global-set-key (kbd "C-h a") 'apropos)

;; Should be able to eval-and-replace anywhere.
(global-set-key (kbd "C-c e") 'eval-and-replace)


;;
;; Misc
;;

(require 'sync-mode)

(require 'saveplace)
(setq save-place-file (concat dotfiles-dir "places"))

(require 'ffap)
(setq ffap-machine-p-known 'reject)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Make those nasty ANSI sequences pretty emacs faces
(require 'ansi-color)
(ansi-color-for-comint-mode-on)

;; Save a list of recent files visited.
(require 'recentf)
(recentf-mode 1)

;; Transparently open compressed files
(auto-compression-mode t)

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

;; ido-mode is like magic pixie dust!
(ido-mode t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-max-prospects 10)

(eval-after-load 'grep
  '(when (boundp 'grep-find-ignored-files)
    (add-to-list 'grep-find-ignored-files "target")
    (add-to-list 'grep-find-ignored-files "*.class")))

;; Default to unified diffs
(setq diff-switches "-u -w"
      magit-diff-options "-w")

;; make emacs use the clipboard
(setq x-select-enable-clipboard t)

;; no weird mumamo colors
(setq mumamo-background-colors nil)

(message "init.el: My .emacs loaded in %.1fs" (- (float-time) *emacs-load-start*))
