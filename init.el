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
                      which-key))

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

;; Buffer cycling
(defun sindriava/next-buffer ()
  (interactive)
  (let ((bread-crumb (buffer-name)))
    (next-buffer)
    (while
        (and
         (not (equal "*scratch*" (buffer-name)))
         (string-match-p "^\*" (buffer-name))
         (not (equal bread-crumb (buffer-name))))
      (next-buffer))))

(defun sindriava/previous-buffer ()
  (interactive)
  (let ((bread-crumb (buffer-name)))
    (previous-buffer)
    (while
        (and
         (not (equal "*scratch*" (buffer-name)))
         (string-match-p "^\*" (buffer-name))
         (not (equal bread-crumb (buffer-name))))
      (previous-buffer))))

(global-set-key (kbd "M-[") 'sindriava/previous-buffer)
(global-set-key (kbd "M-]") 'sindriava/next-buffer)

;; Various keybindings
(global-set-key (kbd "C-j") (lambda () (interactive) (join-line -1)))
(global-set-key (kbd "C-x k") (lambda () (interactive) (kill-buffer (buffer-name))))

;; Prettier cursor
(setq-default cursor-type 'bar)
(set-cursor-color "#F92672")

;; Modeline
(defvar mode-line-cleaner-alist
  `(;;Minor modes
    (paredit-mode   . " p")
    (company-mode   . " c")
    (which-key-mode . nil)
    ;; Major modes
    (lisp-interaction-mode . "λ")
    (emacs-lisp-mode	   . "λ")
    (haskell-mode	   . ">>=")))

(defun clean-mode-line ()
  (interactive)
  (mapcar (lambda (cleaner)
	    (let* ((mode (car cleaner))
		   (mode-str (cdr cleaner))
		   (old-mode-str (cdr (assq mode minor-mode-alist))))
	      (when old-mode-str
		(setcar old-mode-str mode-str))
	      ;; major mode
	      (when (eq mode major-mode)
		(setq mode-name mode-str))))
	  mode-line-cleaner-alist))

(add-hook 'after-change-major-mode-hook 'clean-mode-line)

(setq-default mode-line-format
	      '("%e"
		(:eval (propertize " %m " 'face '(:foreground "#F92672")))
		(:eval (propertize "%b"   'face '(:foreground "#E6DB74")))))

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)

;; Paredit
(autoload 'enable-paredit-mode "paredit" t)

(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)

;; Disable some annoying paredit bindings
(eval-after-load "paredit"
  (progn
    (define-key paredit-mode-map (kbd "C-j") nil)))

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
(require 'dired-x)

(put 'dired-find-alternate-file 'disabled nil)
(setq dired-omit-files "^.DS_Store$")
(add-hook 'dired-mode-hook (lambda ()
			     ;; Hide unnecessary files
			     (dired-omit-mode 1)
			     ;; hide the cursor
			     (hl-line-mode)
			     (setq-local cursor-type nil)))

(with-eval-after-load "dired"
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "^") (lambda ()
					 (interactive)
					 (find-alternate-file "..")))

  (define-key dired-mode-map (kbd "k") 'dired-previous-line)
  (define-key dired-mode-map (kbd "j") 'dired-next-line)
  (define-key dired-mode-map (kbd "g") 'dired-jump)
  (define-key dired-mode-map (kbd "r") 'revert-buffer))

;; Backups & custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror 'nomessage)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
