(setq default-frame-alist '((width  . 80)
			    (height . 25)
			    (font   . "Input-16")))

;; Emacs pls
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message "sindriava")
;; (setq initial-scratch-message "")

(defalias 'yes-or-no-p 'y-or-n-p)

;; Hide redundant UI elements
(tool-bar-mode 0)
(scroll-bar-mode 0)

(setq ring-bell-function 'ignore)
(set-default 'cursor-type 'bar)
;; (setq x-select-enable-clipboard nil)

;; Split vertically by default
(setq split-width-threshold nil)
(setq split-width-threshold 1)

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

(defvar my-packages '(magit
                      paredit
                      company
		      haskell-mode))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Custom modal editing
(defvar navigation-mode-map (make-sparse-keymap))

(define-key navigation-mode-map (kbd "h") 'backward-char)
(define-key navigation-mode-map (kbd "j") 'next-line)
(define-key navigation-mode-map (kbd "k") 'previous-line)
(define-key navigation-mode-map (kbd "l") 'forward-char)

(define-minor-mode navigation-mode
  "Navigation mode" nil "<N>"
  navigation-mode-map
  (setq-local cursor-type
              (if navigation-mode
                  'box
                  (default-value 'cursor-type))))

(define-key global-map (kbd "<escape>") 'navigation-mode)

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)

;; Paredit
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)

(add-hook 'ielm-mode-hook #'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)

;; Haskell
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

(require 'haskell-interactive-mode)
(require 'haskell-process)

(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

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
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Custom shit
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (paredit magit haskell-mode company))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
