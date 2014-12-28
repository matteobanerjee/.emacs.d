;;;;;;;;;;;;;;
;; Packages ;;
;;;;;;;;;;;;;;

(when (version<= emacs-version "24")
  (message "Your Emacs is too old, update to emacs 24"))

(require 'package)

(setq package-list '(color-theme
                     color-theme-solarized
                     ensime
                     ido-vertical-mode
                     magit
                     markdown-mode
                     flycheck
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
(require 'color-theme)
(color-theme-solarized-dark)
(require 'flycheck)

(setq linum-format "%3d ")
(global-linum-mode t)
(show-paren-mode t)
(ido-mode t)
(ido-vertical-mode t)
(yas-global-mode t)

;;;;;;;;;;;
;; Scala ;;
;;;;;;;;;;;

(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
(add-hook 'scala-mode-hook
          '(lambda()
             (set (make-local-variable 'whitespace-line-column) 100)))

;;;;;;;;;;
;; Java ;;
;;;;;;;;;;
(add-hook 'java-mode-hook
          '(lambda()
             (set (make-local-variable 'whitespace-line-column) 100)))
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
 '(custom-safe-themes (quote ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" default))))

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
