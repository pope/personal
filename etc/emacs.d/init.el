;; A lot of this is stolen from emacs-starter-kit.  I'm pulling out
;; the bits I like!

(defvar *emacs-load-start* (float-time))


;;
;; The Basics
;;

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
      generated-autoload-file autoload-file)
(setq-default indent-tabs-mode nil)

;; Don't clutter up directories with files~
(setq backup-directory-alist `(("." . ,(expand-file-name
                                        (concat dotfiles-dir "backups")))))

(load custom-file 'noerror)


;;
;; libs
;;

;; For ECB.
(setq stack-trace-on-error t)

(setq package-user-dir "~/.emacs.d/elpa")
(setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))

(when (file-exists-p (concat dotfiles-dir "libs/init.el"))
  (load (concat dotfiles-dir "libs/init")))

(message "init.el: libs loaded after %.1fs" (- (float-time) *emacs-load-start*))


;;
;; Defuns
;;

(require 'thingatpt)
(require 'imenu)

(defun ido-goto-symbol (&optional symbol-list)
  "Refresh imenu and jump to a place in the buffer using Ido."
  (interactive)
  (unless (featurep 'imenu)
    (require 'imenu nil t))
  (cond
   ((not symbol-list)
    (let ((ido-mode ido-mode)
          (ido-enable-flex-matching
           (if (boundp 'ido-enable-flex-matching)
               ido-enable-flex-matching t))
          name-and-pos symbol-names position)
      (unless ido-mode
        (ido-mode 1)
        (setq ido-enable-flex-matching t))
      (while (progn
               (imenu--cleanup)
               (setq imenu--index-alist nil)
               (ido-goto-symbol (imenu--make-index-alist))
               (setq selected-symbol
                     (ido-completing-read "Symbol? " symbol-names))
               (string= (car imenu--rescan-item) selected-symbol)))
      (unless (and (boundp 'mark-active) mark-active)
        (push-mark nil t nil))
      (setq position (cdr (assoc selected-symbol name-and-pos)))
      (cond
       ((overlayp position)
        (goto-char (overlay-start position)))
       (t
        (goto-char position)))))
   ((listp symbol-list)
    (dolist (symbol symbol-list)
      (let (name position)
        (cond
         ((and (listp symbol) (imenu--subalist-p symbol))
          (ido-goto-symbol symbol))
         ((listp symbol)
          (setq name (car symbol))
          (setq position (cdr symbol)))
         ((stringp symbol)
          (setq name symbol)
          (setq position
                (get-text-property 1 'org-imenu-marker symbol))))
        (unless (or (null position) (null name)
                    (string= (car imenu--rescan-item) name))
          (add-to-list 'symbol-names name)
          (add-to-list 'name-and-pos (cons name position))))))))

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
  (if (fboundp 'paredit-mode) (paredit-mode t)))

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

(defun json-pretty-print-buffer ()
  (interactive)
  (shell-command-on-region (point-min) (point-max) "python -mjson.tool" t)
  (js-mode))



;;
;; Coding Basics
;;

(defvar pope-buffer-remove-trailing-whitespace nil
  "If non-nil, remove trailing whitespace in this buffer when saving.")
(make-variable-buffer-local 'pope-buffer-remove-trailing-whitespace)

(defun pope-maybe-delete-trailing-whitespace ()
  (and pope-buffer-remove-trailing-whitespace
       (fboundp 'delete-trailing-whitespace)
       (delete-trailing-whitespace)))

;; TODO: Add the coding hooks
(add-hook 'before-save-hook #'pope-maybe-delete-trailing-whitespace)


;;
;; Eshell
;;

(setq eshell-cmpl-cycle-completions nil
      eshell-save-history-on-exit t
      eshell-hist-ignoredups t
      eshell-history-size 512
      eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\|\\.svn\\|\\.git\\)/\\'")

(defun eshell/find (dir &rest opts)
  (find-dired dir (mapconcat 'identity opts " ")))

(defun eshell/clear ()
  "04Dec2001 - sailor, to clear the eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(defun eshell/which-git-branch ()
  (shell-command-to-string "git branch --no-color 2> /dev/null | grep \"*\" | awk '{print $2}'"))

(defun my-eshell-prompt-function ()
  (let ((branch (replace-regexp-in-string
                 "\\(^[[:space:]\n]*\\|[[:space:]\n]*$\\)"
                 ""
                 (eshell/which-git-branch))))
    (concat (abbreviate-file-name (eshell/pwd))
            (if (not (equal "" branch)) (concat " (" branch ")") "")
            (if (= (user-uid) 0) " # " " $ "))))

(eval-after-load 'esh-opt
  '(progn
     (require 'em-prompt)
     (require 'em-term)
     (require 'em-cmpl)
     (setenv "PAGER" "cat")
     ;;(set-face-attribute 'eshell-prompt nil :foreground "turquoise1")
     (add-hook 'eshell-mode-hook ;; for some reason this needs to be a hook
               '(lambda () (eshell/export "TERM" "dumb")))
     (when (< emacs-major-version 23)
       (add-hook 'eshell-mode-hook ;; for some reason this needs to be a hook
                 '(lambda () (define-key eshell-mode-map "\C-a" 'eshell-bol)))
       (add-to-list 'eshell-output-filter-functions 'eshell-handle-ansi-color))

     ;; TODO: submit these via M-x report-emacs-bug
     (add-to-list 'eshell-visual-commands "ssh")
     (add-to-list 'eshell-visual-commands "tail")
     (add-to-list 'eshell-visual-commands "vim")
     (add-to-list 'eshell-command-completions-alist
                  '("gunzip" "gz\\'"))
     (add-to-list 'eshell-command-completions-alist
                  '("tar" "\\(\\.tar|\\.tgz\\|\\.tar\\.gz\\)\\'"))))

(eval-after-load 'em-prompt
  '(progn
     (setq eshell-prompt-function 'my-eshell-prompt-function)))


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
;; HTML
;;

(setq nxml-bind-meta-tab-to-complete-flag t
      nxml-slash-auto-complete-flag t)


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
     (add-hook 'php-mode-hook 'php-mode-settings)
     (add-hook 'php-mode-hook 'turn-on-zf)))

(eval-after-load "geben"
  (setenv "XDEBUG_CONFIG" "idekey=geben_session"))


;;
;; C
;;

;; http://mike.struct.cn/blogs/entry/15/

(defun my-c-mode-common-hook()
  (setq pope-buffer-remove-trailing-whitespace t)
  (when (featurep 'auto-complete)
    (setq ac-auto-start nil)
    (setq ac-expand-on-auto-complete nil)
    (setq ac-quick-help-delay 0.3)
    (setq ac-sources (append '(ac-source-clang) ac-sources))
    (define-key c-mode-base-map (kbd "M-/") #'ac-complete-clang)))

(add-hook 'c-mode-hook #'my-c-mode-common-hook)


;;
;; Objective-C
;;

(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'objc-mode))

(defun my-objc-defun-block-intro (langelem)
  (when (derived-mode-p 'objc-mode)
    (save-excursion
      (let ((parent (save-excursion
                      (goto-char (c-langelem-pos langelem))
                      (c-guess-basic-syntax))))
        (cond ((assq 'objc-method-intro parent) c-basic-offset)
              (t (back-to-indentation)
                 ;; Go to beginning of *previous* line:
                 (c-backward-syntactic-ws)
                 (back-to-indentation)
                 (vector (+ (* 2 (if (integerp c-basic-offset) c-basic-offset 2))
                            (current-column)))))))))

(defun my-objc-defun-close (langelem)
  (when (derived-mode-p 'objc-mode)
    (save-excursion
      (back-to-indentation)
      (c-go-up-list-backward)
      (back-to-indentation)
      (when (assq 'objc-method-call-cont (c-guess-basic-syntax))
        (vector (current-column))))))

(defun my-objc-mode-hook ()
  (setq c-basic-offset 2)
  (setq pope-buffer-remove-trailing-whitespace t)
  (c-set-offset 'access-label '/)
  (c-set-offset 'defun-block-intro #'my-objc-defun-block-intro '++)
  (c-set-offset 'defun-close #'my-objc-defun-close)
  (c-set-offset 'statement-cont '++))

(eval-after-load "cc-mode"
  '(progn
     (font-lock-add-keywords 'objc-mode
                             '(("\\<@\\(synthesize\\|property\\|required\\|optional\\|dynamic\\)\\>" . font-lock-keyword-face)))
     (add-hook 'objc-mode-hook #'my-objc-mode-hook)))

(defun my-objc-match-function ()
  (and (string= (file-name-extension buffer-file-name) "h")
       (re-search-forward "@\\(implementation\\|interface\\|protocol\\)"
                          magic-mode-regexp-match-limit
                          t)))

(add-to-list 'magic-mode-alist '(my-objc-match-function . objc-mode))

(eval-after-load "find-file"
  '(progn
     (nconc (cadr (assoc "\\.h\\'" cc-other-file-alist)) '(".m" ".mm"))
     (add-to-list 'cc-other-file-alist '("\\.m\\'" (".h")))
     (add-to-list 'cc-other-file-alist '("\\.mm\\'" (".h")))))


;;
;; go
;;

(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'go-mode))

(defun my-go-mode-hook ()
  (setq fill-column 78)
  (setq tab-width 2)
  (auto-complete-mode 1))

(eval-after-load "go-mode"
  '(progn
     (add-hook 'go-mode-hook #'my-go-mode-hook)
     (add-hook 'before-save-hook #'gofmt-before-save)))


;;
;; Octave
;;

(add-hook 'inferior-octave-mode-hook
          (lambda ()
            (turn-on-font-lock)
            (define-key inferior-octave-mode-map [up]
              'comint-previous-input)
            (define-key inferior-octave-mode-map [down]
              'comint-next-input)))


;;
;; JavaScript
;;

(defun js-sort-goog-requires ()
  (interactive)
  (with-current-buffer (buffer-name)
      (save-excursion
        (goto-char (point-min))
        (re-search-forward "^goog.require")
        (beginning-of-line)
        (sort-lines nil
                    (point)
                    (save-excursion
                      (while (looking-at-p "^goog.require")
                        (next-line))
                      (line-end-position))))))

(defun my-js2-mode-hook ()
  (setq pope-buffer-remove-trailing-whitespace t))

(eval-after-load "js2-mode"
  (add-hook 'js2-mode-hook #'my-js2-mode-hook))

(eval-after-load "js-mode"
  (add-hook 'js-mode-hook #'my-js2-mode-hook))


;;
;; Org-mode
;;

(defun my-org-redisplace-inline-images ()
  (org-display-inline-images))

(add-hook 'org-babel-after-execute-hook 'my-org-redisplace-inline-images)

(eval-after-load 'ob
  '(progn
     (org-babel-do-load-languages
      'org-babel-load-languages
      '((C . t)
        (css . t)
        (ditaa . t)
        (dot . t)
        (emacs-lisp . t)
        (gnuplot . t)
        (go . t)
        (java . t)
        (js . t)
        (lisp . t)
        (octave . t)
        (plantuml . t)
        (python . t)
        (ruby . t)
        (sh . t)
        (sql . t)
        (sqlite . t)))))


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
(global-set-key (kbd "M-%") 'query-replace-regexp)
(global-set-key (kbd "C-M-%") 'query-replace)


;; Jump to a definition in the current file. (This is awesome.)
(global-set-key (kbd "C-x C-i") 'ido-goto-symbol)

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

(if (string-equal system-type "darwin")
    (setq x-alt-keysym 'meta))


;;
;; Tramp
;;

(eval-after-load 'tramp
  '(progn
     ;; Disable vc over tramp.
     (setq vc-ignore-dir-regexp
           (format "\\(%s\\)\\|\\(%s\\)"
                   vc-ignore-dir-regexp
                   tramp-file-name-regexp))
     (setq tramp-auto-save-directory temporary-file-directory)))


;;
;; Misc
;;

(require 'saveplace)
(setq save-place-file (concat dotfiles-dir "places"))

(eval-after-load 'ffap
  '(setq ffap-machine-p-known 'reject))

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Make those nasty ANSI sequences pretty emacs faces
(require 'ansi-color)
(ansi-color-for-comint-mode-on)

;; Save a list of recent files visited.
(require 'recentf)
(setq recentf-keep '(file-remote-p file-readable-p))
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
(setq diff-switches "-u -w")

;; make emacs use the clipboard
(setq x-select-enable-clipboard t)

;; no weird mumamo colors
(setq mumamo-background-colors nil)

;; I can use the "a" key in dired mode...I'm cool with that
(put 'dired-find-alternate-file 'disabled nil)

;; Save the minibuffer history
(savehist-mode 1)

;; I hate the auto window splitting.  I'll control that thank you.
(setq split-width-threshold most-positive-fixnum    ;; was 160
      split-height-threshold most-positive-fixnum)  ;; was 80

;; Better Cursor Support
(defun djcb-set-cursor-according-to-mode ()
  "change cursor color and type according to some minor modes."
  (cond
   (buffer-read-only
    (setq cursor-type 'hbar))
   (overwrite-mode
    (setq cursor-type 'box))
   (t
    (setq cursor-type 'bar))))

(add-hook 'post-command-hook 'djcb-set-cursor-according-to-mode)

;; Some disabled warnings.
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

;; Make sure that $PATH matches the exec-path that I have already specified.
(setenv "PATH" (mapconcat 'identity exec-path ":"))

(message "init.el: My .emacs loaded in %.1fs" (- (float-time) *emacs-load-start*))

;; load in a host specific file if it's there
(setq system-specific-config (concat dotfiles-dir system-name ".el"))
(if (file-exists-p system-specific-config)
    (progn
      (load system-specific-config)
      (message "init.el: %s loaded after %.1fs" (concat system-name ".el") (- (float-time) *emacs-load-start*))))
(put 'upcase-region 'disabled nil)
