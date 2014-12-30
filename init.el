;;;;;;;;;;;;;;
;; Packages ;;
;;;;;;;;;;;;;;

(when (version<= emacs-version "24")
  (message "Your Emacs is too old, update to emacs 24"))

(require 'package)

(setq package-list '(color-theme
                     color-theme-solarized
                     ensime
                     flycheck
                     helm
                     magit
                     markdown-mode
                     mvn
                     neotree
                     thrift
                     yasnippet))

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(or (file-exists-p package-user-dir)
    (package-refresh-contents))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; General Preferences ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'neotree)
(require 'helm-config)

(require 'color-theme)
(color-theme-solarized-dark)

(require 'flycheck)
(setq linum-format "%3d ")
(global-linum-mode t)
(show-paren-mode t)
(helm-mode 1)
(yas-global-mode t)

;;;;;;;;;;;
;; Scala ;;
;;;;;;;;;;;

(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
(add-hook 'scala-mode-hook
          '(lambda() (set (make-local-variable 'whitespace-line-column) 100)))

;;;;;;;;;;
;; Java ;;
;;;;;;;;;;
(add-hook 'java-mode-hook
          '(lambda() (set (make-local-variable 'whitespace-line-column) 100)))
;;;;;;;;;;;;
;; Python ;;
;;;;;;;;;;;;

(add-hook 'python-mode-hook 'flycheck-mode)
;; TODO: make ipython default shell

;;;;;;;;;;
;; Ruby ;;
;;;;;;;;;;

(add-hook 'ruby-mode-hook 'flycheck-mode)

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

;;;;;;;;;;;;;
;; Display ;;
;;;;;;;;;;;;;

(setq whitespace-style
  (quote (face trailing tab-mark lines-tail)))
(add-hook 'find-file-hook 'whitespace-mode)

;; transform literal tabs into a right-pointing triangle
;http://ergoemacs.org/emacs/whitespace-mode.html
(setq whitespace-display-mappings
 '((tab-mark 9 [9654 9] [92 9])))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" default))))

;; Disable toolbars
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;;;;;;;;;;;;;;;;;;
;; Backup Files ;;
;;;;;;;;;;;;;;;;;;

;; Fix issue w/ missing function in emacs 24
;; http://stackoverflow.com/questions/11690980/why-cant-i-use-old-theme-style-file-under-emacs-24-1/14183331#14183331
(if (>= emacs-major-version 24)
    (defun plist-to-alist (the-plist)
      (defun get-tuple-from-plist (the-plist)
        (when the-plist
          (cons (car the-plist) (cadr the-plist))))

      (let ((alist '()))
        (while the-plist
          (add-to-list 'alist (get-tuple-from-plist the-plist))
          (setq the-plist (cddr the-plist)))
        alist)))

;; backup files in /.emacs.d/backups
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.emacs.d/backups"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)


;; ADD THE FOLLOWING TO solarized-definitions.el
;; FROM: https://github.com/sellout/emacs-color-theme-solarized/commit/5a63acf27172cd40a79b6cbdb5f0f9181861ec99
;; (helm-action ((t (,@fmt-undr))))
;; (helm-apt-deinstalled ((t (,@fg-base01))))
;; (helm-apt-installed ((t (,@fg-green))))
;; (helm-bookmark-addressbook ((t (,@fg-blue))))
;; (helm-bookmark-directory ((t (:inherit helm-ff-directory))))
;; (helm-bookmark-file ((t (:inherit helm-ff-file))))
;; (helm-bookmark-gnus ((t (,@fg-cyan))))
;; (helm-bookmark-info ((t (,@fg-green))))
;; (helm-bookmark-man ((t (,@fg-violet))))
;; (helm-bookmark-w3m ((t (,@fg-yellow))))
;; (helm-bookmarks-su ((t (,@fg-orange))))
;; (helm-buffer-not-saved ((t (,@fg-orange))))
;; (helm-buffer-process ((t (,@fg-magenta))))
;; (helm-buffer-saved-out ((t (,@fmt-revr ,@fg-red ,@bg-back))))
;; (helm-buffer-size ((t (,@fg-base01))))
;; (helm-candidate-number ((t (,@fmt-bold ,@bg-base02 ,@fg-base1))))
;; (helm-emms-playlist ((t (,@fmt-none ,@fg-base01))))
;; (helm-etags+-highlight-face ((t (:inherit highlight))))
;; (helm-ff-directory ((t (,@bg-back ,@fg-blue))))
;; (helm-ff-executable ((t (,@fmt-bold ,@fg-green))))
;; (helm-ff-file ((t (:inherit default))))
;; (helm-ff-invalid-symlink ((t (,@bg-base02 ,@fg-red))))
;; (helm-ff-prefix ((t (,@bg-yellow :foreground ,back))))
;; (helm-ff-symlink ((t (,@fmt-bold ,@fg-cyan))))
;; (helm-gentoo-match ((t (:inherit helm-match))))
;; (helm-grep-cmd-line ((t (:inherit diff-added))))
;; (helm-grep-file ((t (,@fmt-undr ,@fg-cyan))))
;; (helm-grep-finish ((t (,@fg-green))))
;; (helm-grep-lineno ((t (,@fg-orange))))
;; (helm-grep-match ((t (:inherit helm-match))))
;; (helm-grep-running ((t (,@fg-red))))
;; (helm-header ((t (:inherit header-line))))
;; (helm-helper ((t (:inherit helm-header))))
;; (helm-history-deleted ((t (:inherit helm-ff-invalid-symlink))))
;; (helm-history-remote ((t (,@fg-red))))
;; (helm-lisp-completion-info ((t (,@fg-base0))))
;; (helm-lisp-show-completion ((t (,@fmt-bold ,@fg-yellow  ,@bg-base02))))
;; (helm-ls-git-added-copied-face ((t (,@fg-green))))
;; (helm-ls-git-conflict-face ((t (,@fmt-bold ,@fg-red))))
;; (helm-ls-git-deleted-and-staged-face ((t (,@fmt-ital ,@fg-base01))))
;; (helm-ls-git-deleted-not-staged-face ((t (,@fmt-bold ,@fg-green))))
;; (helm-ls-git-modified-and-staged-face ((t (,@fmt-ital ,@fg-base01))))
;; (helm-ls-git-modified-not-staged-face ((t (,@fmt-ital ,@fg-base01))))
;; (helm-ls-git-renamed-modified-face ((t (,@fg-green))))
;; (helm-ls-git-untracked-face ((t (,@fg-red))))
;; (helm-M-x-key ((t (,@fmt-undr ,@fg-orange))))
;; (helm-match ((t (,@fmt-bold ,@fg-base1 ,@bg-base02))))
;; (helm-moccur-buffer ((t (,@fmt-undr ,@fg-cyan))))
;; (helm-selection ((t (,@fmt-undr ,@bg-base02))))
;; (helm-selection-line ((t (,@bg-base02 ,@fg-base1))))
;; (helm-separator ((t (,@fg-red))))
;; (helm-source-header ((t (:inherit helm-header))))
;; (helm-time-zone-current ((t (,@fg-green))))
;; (helm-time-zone-home ((t (,@fg-red))))
;; (helm-visible-mark ((t (,@fmt-bold ,@bg-back ,@fg-magenta))))
;; (helm-w3m-bookmarks ((t (:inherit helm-bookmark-w3m))))

