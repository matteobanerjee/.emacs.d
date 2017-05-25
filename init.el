;;;;;;;;;;;;;;
;; Packages ;;
;;;;;;;;;;;;;;

(when (version<= emacs-version "24")
  (message "Your Emacs is too old, update to emacs 24"))

(require 'package)
(setq package-list '(auto-complete
                     cider
                     clojure-mode
                     paredit
                     expand-region
                     ensime
                     flycheck
		     go-mode
		     go-autocomplete
                     haskell-mode
                     helm
                     helm-c-yasnippet
                     js2-mode
                     magit
                     markdown-mode
                     monokai-theme
                     mvn
                     nodejs-repl
                     projectile
                     projectile-rails
                     powerline
                     rainbow-mode
                     neotree
                     thrift
                     tern
                     tern-auto-complete
                     yaml-mode
                     yasnippet))

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
(setq font-use-system-font t)
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(setq x-select-enable-clipboard t)
(define-key input-decode-map "\e\eOA" [(meta up)])
(define-key input-decode-map "\e\eOB" [(meta down)])

;;;;;;;;;;;;;;;;;;;;;;;;;
;;General Preferences ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'neotree)
(require 'helm-config)
(require 'yaml-mode)
(require 'powerline)

;; color theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/orpheus-theme/")
(load-theme 'wombat t)
;; (load-theme 'whiteboard t)
(powerline-default-theme)
;; For working on custom themes
(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))
(require 'rainbow-mode)

(require 'flycheck)
(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

(setq indent-tabs-mode nil)
;; when using powerline linum mode is less important
;; (setq linum-format "%3d")
;; (global-linum-mode t)
(show-paren-mode t)
;; (setq show-paren-style 'expression)
(helm-mode 1)
(helm-autoresize-mode t)
(setq helm-yas-space-match-any-greedy t) ;[default: nil]
(global-set-key (kbd "C-c y") 'helm-yas-complete)
(yas-global-mode t)

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;;;;;;;;;;;
;; Scala ;;
;;;;;;;;;;;
;; Make ensime work in OS X Emacs.app
(setq exec-path (append exec-path '("/usr/local/bin")))
(setq exec-path (append exec-path '("/usr/local/sbin")))
(setenv "PATH" (shell-command-to-string "/bin/bash -l -c 'echo -n $PATH'"))

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
(add-hook 'ruby-mode-hook 'projectile-mode)
(add-hook 'projectile-mode-hook 'projectile-rails-on)

;;;;;;;;;;;;;
;; Haskell ;;
;;;;;;;;;;;;;

(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;;;;;;;;;;;;;;;;
;; JavaScript ;;
;;;;;;;;;;;;;;;;
(add-hook 'js2-mode-hook 'flycheck-mode)
(add-hook 'js2-jsx-mode-hook 'flycheck-mode)
(flycheck-add-mode 'javascript-eslint 'js2-mode)
(flycheck-add-mode 'javascript-eslint 'js2-jsx-mode)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-jsx-mode))

(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
          '(javascript-jshint)))


(setq js2-mode-hook
      '(lambda () (progn (set-variable 'indent-tabs-mode nil))))
(add-hook 'js2-mode-hook 'tern-mode)
(add-hook 'js2-mode-hook 'auto-complete-mode)
          ;; (lambda () (tern-mode t)))

(eval-after-load 'tern
  '(progn
     (require 'tern-auto-complete)
           (tern-ac-setup)))
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

(with-eval-after-load 'go-mode
  (require 'go-autocomplete))

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
  (quote (face trailing lines-tail)))
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

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:strike-through nil :overline nil :underline nil :slant normal :weight normal :height 130 :width normal :foundry "nil" :family "Menlo")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#1c1c1c" "#ff4b4b" "#d7ff5f" "#fce94f" "#5fafd7" "#d18aff" "#afd7ff" "#c6c6c6"])
 '(compilation-message-face (quote default))
 '(custom-safe-themes
   (quote
    ("28130127bbf3072c1bbc7652fca7245f186bb417b3b385a5e4da57b895ffe9d8" "75caffa869a4a4a0ed818bfc1923ebae1c5a2519c5d96b03b0e8e88a7bcf731a" "05c3bc4eb1219953a4f182e10de1f7466d28987f48d647c01f1f0037ff35ab9a" "58bc69893f1ec4ff5561a8995e7e2227d4f04ca5285362072543b6458ed08532" "3b94bef8463fff8d110f9093e3fa9f2f4be7d8675f348431c993f0e5f2c4f2da" "70b00f112d2beb190e8506d5a026bf8b1527b5b8f1d8d43602af28180ab6fa99" "68c210a131110b74cd5477f44488e582d72133de058cb22dde4d258591095c48" "27bb4abf8f61fb2f78febfbd069bc9c754d22ed041b496b2e22cd3f6e27b407f" "d8fdbac19ec094486ea90e85daa0a32c07ebf3a4e50f819cb4372462a8bfd59c" "309a4ebfaf88262a2ce2228180677bf68495bc1aa391c7b9a5fbaf41d1ca7fa7" "0a8c661f1e755fb558fc826f9a9a670f28501a4c2e5cd118ec59a402da78898c" "d87c1b9b33fff049f3ea43cf173ba218bd9d6e0c0953b867e7a98683c4cab1d9" "70f1381b8b38a7e4b694b29f3bc217115c28056d8a8d64f2d7620fb8bd383dc1" "75fa3d1e9df3ad793838f8776aae175ec96405c4f3863e171102aca8366a3451" "0661d77770080767c9317446c6cb31e3aa351148b9e388c798318853361f6f5d" "90875daaf9fabcad8c209a522b329ac4affb52456d99311d567cfc537ee087a0" "1cd9defef2a98138c732728568b04043afd321eb802d25a254777de9b2463768" default)))
 '(fci-rule-color "#a8a8a8")
 '(highlight-changes-colors ("#FD5FF0" "#AE81FF"))
 '(highlight-tail-colors
   (quote
    (("#49483E" . 0)
     ("#67930F" . 20)
     ("#349B8D" . 30)
     ("#21889B" . 50)
     ("#968B26" . 60)
     ("#A45E0A" . 70)
     ("#A41F99" . 85)
     ("#49483E" . 100))))
 '(js-indent-level 2)
 '(magit-diff-use-overlays nil)
 '(package-selected-packages
   (quote
    (yaml-mode tern-auto-complete tern thrift neotree rainbow-mode powerline projectile-rails projectile nodejs-repl mvn monokai-theme markdown-mode magit js2-mode helm-c-yasnippet helm haskell-mode flycheck ensime expand-region paredit cider auto-complete)))
 '(safe-local-variable-values
   (quote
    ((haskell-process-use-ghci . t)
     (haskell-indent-spaces . 4)
     (eval when
	   (require
	    (quote rainbow-mode)
	    nil t)
	   (rainbow-mode 1)))))
 '(vc-annotate-background "#202020")
 '(vc-annotate-color-map
   (quote
    ((20 . "#a40000")
     (40 . "#dd0000")
     (60 . "#ff8700")
     (100 . "#ffad00")
     (120 . "#ffd700")
     (140 . "#8abd00")
     (160 . "#9cd700")
     (180 . "#9cd700")
     (200 . "#afff00")
     (220 . "#bdf800")
     (240 . "#bdff0b")
     (260 . "#87d7af")
     (280 . "#79ADB0")
     (300 . "#89C5C8")
     (320 . "#5fafd7")
     (340 . "#9CC7FB")
     (360 . "#c61b6e"))))
 '(vc-annotate-very-old-color "#c61b6e")
 '(weechat-color-list
   (unspecified "#272822" "#49483E" "#A20C41" "#F92672" "#67930F" "#A6E22E" "#968B26" "#E6DB74" "#21889B" "#66D9EF" "#A41F99" "#FD5FF0" "#349B8D" "#A1EFE4" "#F8F8F2" "#F8F8F0")))
