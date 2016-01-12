(setq default-frame-alist '((width  . 80)
			    (height . 25)
			    (font   . "Input-16")))

;; Emacs pls
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message "sindriava")
;; (setq initial-scratch-message "")

(defalias 'yes-or-no-p 'y-or-n-p)

;; (setq x-select-enable-clipboard nil)

;; Hide redundant UI elements
(tool-bar-mode 0)
(scroll-bar-mode 0)

(setq ring-bell-function 'ignore)

;; Sensitive scrolling
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq scroll-step 1)

;; Don't wrap lines
(setq-default truncate-lines 1)

;; Don't indent with tabs
(setq indent-tabs-mode nil)

;; Package setup
(require 'cl)
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(god-mode
                      magit
                      paredit
                      company
                      haskell-mode))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)

;; God mode
(require 'god-mode)
(global-set-key (kbd "<escape>") 'god-local-mode)

(defun my-update-cursor ()
  (let ((limited-colors-p (> 257 (length (defined-colors)))))
    (cond (god-local-mode (progn
                            (set-face-background 'mode-line (if limited-colors-p "white" "#e9e2cb"))
                            (set-face-background 'mode-line-inactive (if limited-colors-p "white" "#e9e2cb"))))
          (t (progn
               (set-face-background 'mode-line (if limited-colors-p "black" "#0a2832"))
               (set-face-background 'mode-line-inactive (if limited-colors-p "black" "#0a2832")))))))

;(add-hook 'god-mode-enabled-hook 'my-update-cursor)
;(add-hook 'god-mode-disabled-hook 'my-update-cursor)

;; Paredit
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
    (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
    (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)

;; Vim movement
;(global-set-key (kbd "C-h") 'backward-char)
;(global-set-key (kbd "C-j") 'next-line)
;(global-set-key (kbd "C-k") 'previous-line)
;(global-set-key (kbd "C-l") 'forward-char)

;; Haskell
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

(require 'haskell-interactive-mode)
(require 'haskell-process)

(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type (quote stack-ghci))
 '(package-selected-packages
   (quote
    (magit markdown-mode paredit god-mode haskell-mode evil-leader company))))

;; (define-key haskell-interactive-mode-map
;;   (kbd "C-l") 'haskell-interactive-mode-clear)

;; Completion
(add-hook 'after-init-hook 'global-company-mode)

;; Theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'base16-eighties-dark t)

(set-face-attribute 'fringe nil
                      :foreground (face-foreground 'default)
                      :background (face-background 'default))

;; Line numbering
(setq linum-format "%3d ")
(global-linum-mode)

;; Backups
(setq backup-directory-alist '(("." . "~/.emacs-backups")))

;; Custom shit

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
