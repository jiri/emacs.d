;; Package setup
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)

;; Theme
(setq default-frame-alist '((width  . 80)
			    (height . 25)
			    (font   . "Input-16")))

(load-theme 'monokai t)

(set-face-attribute 'fringe nil
		    :foreground (face-foreground 'default)
		    :background (face-background 'default))

;; Hide redundant UI elements
(tool-bar-mode 0)
(scroll-bar-mode 0)
(setq frame-title-format nil)
(setq ring-bell-function 'ignore)

;; Automatically install packages
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(dim
		      helm
		      magit
                      paredit
                      company
		      haskell-mode
                      monokai-theme
                      which-key
		      evil))

(defun install-if-not-present (packages)
  (dolist (p packages)
    (when (not (package-installed-p p))
      (package-install p))))

(install-if-not-present my-packages)

;; Emacs pls
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message "sindriava")
;; (setq initial-scratch-message "")

(defalias 'yes-or-no-p 'y-or-n-p)

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

;; Various keybindings
(global-set-key (kbd "C-x k") (lambda () (interactive) (kill-buffer (buffer-name))))
(global-set-key (kbd "M-[") 'previous-buffer)
(global-set-key (kbd "M-]") 'next-buffer)


;; Evil mode
(evil-mode)

(setq evil-default-state 'emacs)
(setq evil-emacs-state-cursor '("#F92672" bar))

(define-key evil-normal-state-map (kbd ";") 'evil-ex)

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)

(with-eval-after-load "evil"
  (add-hook 'with-editor-mode-hook 'evil-insert-state))

;; Paredit
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)

(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)

;; Haskell
(require 'haskell-interactive-mode)
(require 'haskell-process)

(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

;; Completion
(global-company-mode)

;; Prettify symbols
(global-prettify-symbols-mode)

;; Dired mode
(add-hook 'dired-mode-hook (lambda ()
			     (hl-line-mode)
			     (setq-local cursor-type nil)))

(with-eval-after-load "evil"
  (add-to-list 'evil-emacs-state-modes 'dired))

(with-eval-after-load "dired"
  (define-key dired-mode-map (kbd "k") 'dired-previous-line)
  (define-key dired-mode-map (kbd "j") 'dired-next-line)
  (define-key dired-mode-map (kbd "g") 'dired-jump)
  (define-key dired-mode-map (kbd "r") 'revert-buffer))

;; Backups & custom file
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)
