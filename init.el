;;;;;;;;;;;;;;
;; Packages ;;
;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(package-initialize)


(setq package-list
      '(clojure-mode
        paredit
        expand-region
        flycheck
        go-mode
        haskell-mode
        helm
        helm-c-yasnippet
        json-mode
        lsp-mode
        js2-mode
        jsx-mode
        magit
        markdown-mode
        mvn
        neotree
        nodejs-repl
        projectile
        python-mode
        powerline
        racket-mode
        restclient
        rust-mode
        web-mode
        yaml-mode
        yasnippet))


(setq font-use-system-font t)
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(require 'helm-config)
(require 'yaml-mode)
(require 'powerline)
(require 'flycheck)
(require 'expand-region)
(require 'gerbil)
(require 'gambit)
(require 'neotree)
(require 'projectile)

;;;;;;;;;;;;;;;;;
;; Color Theme ;;
;;;;;;;;;;;;;;;;;

(setq load-prefer-newer t)         ;; helps with company mode

(load-theme 'wombat t)


(powerline-default-theme)

;;;;;;;;;;;;;;;;;;
;; Autocomplete ;;
;;;;;;;;;;;;;;;;;;

;; (add-hook 'after-init-hook 'global-company-mode)

;; (setq company-dabbrev-downcase nil)
;; (setq company-idle-delay 0)

;; (defun indent-or-complete ()
;;   (interactive)
;;   (if (looking-at "\\_>")
;;       (company-complete-common)
;;     (indent-according-to-mode)))

;; (global-set-key "\t" 'indent-or-complete)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General Preferences ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(projectile-mode +1)

(setq-default indent-tabs-mode nil)
(electric-indent-mode +1)
(setq x-select-enable-clipboard t)
(define-key input-decode-map "\e\eOA" [(meta up)])
(define-key input-decode-map "\e\eOB" [(meta down)])

(show-paren-mode t)
(setq show-paren-style 'parenthesis)

(helm-mode 1)
(helm-autoresize-mode t)

(setq helm-yas-space-match-any-greedy t) ;[default: nil]
(global-set-key (kbd "C-c y") 'helm-yas-complete)
(yas-global-mode t)
(global-set-key (kbd "C-=") 'er/expand-region)

;;;;;;;;;;;;
;; Python ;;
;;;;;;;;;;;;
(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))
(setq python-shell-interpreter "python3")
(add-hook 'python-mode-hook 'flycheck-mode)

;; TODO: make ipython default shell

;;;;;;;;;;;
;; Bazel ;;
;;;;;;;;;;;

(add-to-list 'auto-mode-alist '("\\.bzl$" . bazel-mode))
(add-to-list 'auto-mode-alist '("BUILD$" . bazel-mode))
(add-to-list 'auto-mode-alist '("WORKSPACE$" . web-mode))

;;;;;;;;;;;;;
;; Haskell ;;
;;;;;;;;;;;;;

(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;;;;;;;;;;;;;;;;
;; JavaScript ;;
;;;;;;;;;;;;;;;;

;; (add-hook 'js2-mode-hook #'lsp)
;; (add-hook 'js2-mode-hook 'flycheck-mode)

;; (add-hook 'js2-mode-hook 'flow-minor-enable-automatically)
;; ;; (add-hook 'js2-jsx-mode-hook 'flow-minor-enable-automatically)
;; (add-hook 'js2-jsx-mode-hook 'flycheck-mode)
;; (flycheck-add-mode 'javascript-eslint 'js2-mode)
;; (flycheck-add-mode 'javascript-eslint 'js2-jsx-mode)

;; (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;; (add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-jsx-mode))

;; (setq-default flycheck-disabled-checkers
;;   (append flycheck-disabled-checkers
;;           '(javascript-jshint)))

(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ts$" . web-mode))
(setq web-mode-content-types-alist
  '(("jsx" . "\\.js[x]?\\'")))

;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)

;; adjust indents for web-mode to 2 spaces
(defun my-web-mode-hook ()
  "Hooks for Web mode. Adjust indents"
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))
(add-hook 'web-mode-hook  'my-web-mode-hook)

;; for better jsx syntax-highlighting in web-mode
;; - courtesy of Patrick @halbtuerke
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
    (let ((web-mode-enable-part-face nil))
      ad-do-it)
    ad-do-it))

;;;;;;;;;
;; XML ;;
;;;;;;;;;

(setq
 nxml-child-indent 4
 nxml-attribute-indent 4
 nxml-slash-auto-complete-flag t)

;;;;;;;;
;; GO ;;
;;;;;;;;

(defun auto-complete-for-go ()
  (auto-complete-mode 1))
(add-hook 'go-mode-hook 'auto-complete-for-go)

;;;;;;;;;;;
;; Lisps ;;
;;;;;;;;;;;

;; copied from racket mode with modifications
(defconst lambda-char (make-char 'greek-iso8859-7 107)
  "Character inserted by `insert-labmda'.")

(defun insert-lambda ()
  (interactive)
  (insert-char lambda-char 1))

(defconst cust-paren-shapes ;; TODO: remove {?
  '( (?\( ?\[ ?\] )
     (?\[ ?\{ ?\} )
     (?\{ ?\( ?\) ))
  "This is not user-configurable because we expect them have to
  have actual ?\( and ?\) char syntax.")

(defun cycle-paren-shapes ()
  "Cycle the sexpr among () [] {}."
  (interactive)
  (save-excursion
    (unless (eq ?\( (char-syntax (char-after)))
      (backward-up-list))
    (pcase (assq (char-after) cust-paren-shapes)
      (`(,_ ,open ,close)
       (delete-char 1)
       (insert open)
       (backward-char 1)
       (forward-sexp 1)
       (backward-delete-char 1)
       (insert close))
      (_
       (user-error "Don't know that paren shape")))))

(add-to-list 'tags-table-list (format "%s/%s" (getenv "GERBIL_HOME") "src/TAGS"))

(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'racket-mode-hook           #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)
(add-hook 'gerbil-mode-hook           #'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

;; C-c C-. doesn't work in Tilix, rebind racket define to C-c C-d
(add-hook 'racket-mode-hook
          (lambda ()
            (define-key racket-mode-map (kbd "C-c C-d") 'racket-describe)))

;;;;;;;;;;;;;;;;;;
;; Key Bindings ;;
;;;;;;;;;;;;;;;;;;

(global-set-key [f8] 'neotree-toggle)
;; Based on: https://sites.google.com/site/steveyegge2/effective-emacs
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)
(global-set-key [f5] 'call-last-kbd-macro)

;; helm stuff
;; http://tuhdo.github.io/helm-intro.html
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)

;;;;;;;;;;;;;;;
;; Functions ;;
;;;;;;;;;;;;;;;

;; From: https://sites.google.com/site/steveyegge2/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
 "Renames both current buffer and file it's visiting to NEW-NAME."
 (interactive "sNew name: ")
 (let ((name (buffer-name))
       (filename (buffer-file-name)))
 (if (not filename)
     (message "Buffer '%s' is not visiting a file!" name)
 (if (get-buffer new-name)
      (message "A buffer named '%s' already exists!" new-name)
   (progn  (rename-file name new-name 1)  (rename-buffer new-name)
           (set-visited-file-name new-name)  (set-buffer-modified-p nil))))))

(defun move-buffer-file (dir)
 "Moves both current buffer and file it's visiting to DIR."
 (interactive "DNew directory: ")
 (let* ((name (buffer-name))
         (filename (buffer-file-name))
          (dir
            (if (string-match dir "\\(?:/\\|\\\\)$")
                 (substring dir 0 -1) dir))
           (newname (concat dir "/" name)))

 (if (not filename)
     (message "Buffer '%s' is not visiting a file!" name)
   (progn (copy-file filename newname 1) (delete-file filename)
          (set-visited-file-name newname) (set-buffer-modified-p nil) t))))

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;;;;;;;;;;;;;
;; Display ;;
;;;;;;;;;;;;;

(setq whitespace-style
  (quote (face trailing)))
(add-hook 'find-file-hook 'whitespace-mode)

;; transform literal tabs into a right-pointing triangle
;http://ergoemacs.org/emacs/whitespace-mode.html
(setq whitespace-display-mappings
 '((tab-mark 9 [9654 9] [92 9])))


;; Disable toolbars
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;;;;;;;;;;;;;;;;;;
;; Backup Files ;;
;;;;;;;;;;;;;;;;;;

;; backup files in /.emacs.d/backups
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.emacs.d/backups"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)

;;;;;;;;;;;;
;; Custom ;;
;;;;;;;;;;;;
;; Keep custom-set-variables in a different file
(setq-default custom-file (expand-file-name ".custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
