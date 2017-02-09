;; Start the server if it's not running already
(require 'server)
(unless (server-running-p)
  (server-start))

;; Initialize `package'
(require 'package)

(package-initialize nil)

;; Add MELPA to `package-archives'
(unless (assoc "melpa" package-archives)
  (add-to-list 'package-archives
	       '("melpa" . "https://melpa.org/packages/") t))

(unless package-archive-contents
  (package-refresh-contents))

;; Set up `use-package'
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(setq use-package-always-ensure t
      use-package-verbose t)

(setq load-prefer-newer t)

;; Workaround for problems with `package.el' and pre-installed packages
(defun package-from-archive (f &rest args)
  "Make an exception for `org' when checking if package is installed"
  (if (equal (car args) 'org)
      (assq 'org package-alist)
    (apply f args)))

(advice-add 'package-installed-p :around 'package-from-archive)

;; Backups & custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror 'nomessage)

(setq create-lockfiles nil
      backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; What the .emacs.d?! version
;; Write backup files to own directory
;; (setq backup-directory-alist
;;       `(("." . ,(expand-file-name
;;                  (concat user-emacs-directory "backups")))))
;;
;; ;; Make backups of files, even when they're in version control
;; (setq vc-make-backup-files t)

;; Disable annoying side-scrolling messages
(dolist (dir '("-left" "-right"))
  (dolist (mul '("" "double-" "triple-"))
    (global-set-key (kbd (concat "<" mul "wheel" dir ">")) 'ignore)))

;; Mode line cleaner
(defvar mode-line-cleaner-alist nil)

(defun mode-line-clean (mode &optional str)
  "Register a name replacement STR for MODE"
  (push `(,mode . ,str) mode-line-cleaner-alist))

(defun mode-line-cleaner ()
  "Substitute mode names according to `mode-line-cleaner-alist'."
  (let ((cleaner-modes (delete-dups
			(mapcar 'car mode-line-cleaner-alist))))
    (dolist (mode cleaner-modes)
      (let* ((mode-str (cdr (assq mode mode-line-cleaner-alist)))
	     (old-mode-str (cdr (assq mode minor-mode-alist))))
	;; Minor modes
	(when old-mode-str
	  (setcar old-mode-str (concat (when mode-str " ") mode-str)))

	;; Major mode
	(when (eq mode major-mode)
	  (setq mode-name mode-str))))))

(add-hook 'after-change-major-mode-hook 'mode-line-cleaner)

;; `PATH' configuration
(setq shell-file-name "zsh")

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;; Niceities
(defalias 'yes-or-no-p 'y-or-n-p)

(delete-selection-mode)
(setq delete-active-region t)

;; Smoother scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 3)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse t)

;; Set up paredit
(use-package paredit
  :config
  (progn
    (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
    (add-hook 'eval-expression-minibuffer-setup-hook 'enable-paredit-mode)
    (add-hook 'inferior-lisp-mode-hook 'enable-paredit-mode)

    (mode-line-clean 'paredit-mode "π")

    (use-package rainbow-delimiters
      :config
      (add-hook 'paredit-mode-hook 'rainbow-delimiters-mode))))

;; Appearance
(setq default-frame-alist '((width . 80) (height . 25)))

(use-package monokai-theme
  :init
  (setq monokai-use-variable-pitch nil)
  :config
  (load-theme 'monokai t))

(global-prettify-symbols-mode)

;; Font settings
(defun font-available-p (font)
  "Check if FONT is available on the system"
  (member font (font-family-list)))

;; NOTE: Starting Emacs as a daemon doesn't load up a GUI frame, so functions
;; in `init.el' relying on that won't work. In the future, this can be circum-
;; vented using `after-make-frame-functions' to register a callback that gets
;; called on all new frames.

;; (when (font-available-p "Input")
;;   (set-face-attribute 'default nil :font "Input-13"))

(add-to-list 'default-frame-alist '(font . "Input-14"))

;; Disable GUI cruft
(when (fboundp 'tool-bar-mode) (tool-bar-mode 0))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode 0))

(setq frame-title-format nil)
(setq ring-bell-function 'ignore)

;; Disable startup screen
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message "sindriava")

;; Modify file I/O
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Org mode
(use-package org
  :config
  (progn
    ;; Key bindings
    (global-set-key (kbd "C-c c") 'org-capture)

    (setq org-startup-folded 'nofold)
    (setq org-special-ctrl-a/e t)
    (setq org-special-ctrl-k t)

    ;; Files
    (setq org-default-notes-file (concat org-directory "/notes.org"))

    ;; HTML exporting
    (setq org-html-checkbox-type 'html)
    (setq org-html-validation-link nil)

    ;; LaTeX exporting
    (add-to-list 'org-latex-packages-alist '("" "minted"))
    (setq org-latex-listings 'minted)
    (setq org-latex-pdf-process
	  '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))))

;; Magit
(use-package magit
  :defer t
  :bind ("C-c g" . magit-status)
  :config
  (progn
    (defun magit-fullscreen (f &rest args)
      "Open `magit-status' full-screen"
      (window-configuration-to-register :magit-fullscreen)
      (apply f args)
      (delete-other-windows))

    (advice-add 'magit-status :around #'magit-fullscreen)

    (defun magit-quit-session ()
      "Restores the previous window configuration and kills the magit buffer"
      (interactive)
      (kill-buffer)
      (jump-to-register :magit-fullscreen))

    (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

    ;; Customize `magit' faces
    (set-face-attribute 'magit-branch-current nil
                        :box nil)))

;; Keyboard bindings
;; Prevent ⌘ + q from killing the Emacs server
(global-set-key (kbd "s-q") 'save-buffers-kill-terminal)

(defun sindriava/beginning-of-line ()
  "Move to first non-whitespace character on `C-a' first."
  (interactive)
  (let ((old-point (point)))
    (back-to-indentation)
    (when (= (point) old-point)
      (beginning-of-line))))

(global-set-key (kbd "C-a") 'sindriava/beginning-of-line)

(defun sindriava/cycle-until-viable (action)
  "Cycle buffers while the current buffer is disabled."
  (let ((bread-crumb (buffer-name)))
    (funcall action)
    (while
	(and
         (not (equal bread-crumb (buffer-name)))
         (not (equal (buffer-name) "*scratch*"))
         (or (string-match-p "^\*" (buffer-name))))
      (funcall action))))

(defun sindriava/kill-buffer ()
  "Kill buffer (except for `*scratch*') and cycle to the next one."
  (interactive)
  (let ((buffer (buffer-name)))
    (if (equal buffer "*scratch*")
	(message "Cannot kill scratch buffer.")
      (progn
	(sindriava/cycle-until-viable 'next-buffer)
	(kill-buffer buffer)))))

(global-set-key (kbd "C-x k") 'sindriava/kill-buffer)

;; Set up `ido-mode'
(use-package ido
  :config
  (progn
    (setq ido-enable-flex-matching t)

    (use-package smex
      :config
      (global-set-key (kbd "M-x") 'smex))

    (defun match-starred-not-scratch (x)
      (and (not (string= x "*scratch*"))
	   (string-match-p "^\\*.*\\*$" x)))

    (add-to-list 'ido-ignore-buffers 'match-starred-not-scratch)

    (ido-mode)))

;; Prettier cursor
(setq-default cursor-type 'bar)
;;(set-cursor-color "#F92672")
(add-to-list 'default-frame-alist '(cursor-color . "#F92672"))

;; Plugins
(use-package hydra)

(use-package expand-region
  :init
  (setq expand-region-fast-keys-enabled nil)
  :config
  (with-eval-after-load 'hydra
    ;; TODO - Make these mode-dependent?
    (defhydra hydra-mark
      (:body-pre (call-interactively 'set-mark-command))
      ;; Rectangle selection
      ("r" rectangle-mark-mode)

      ;; Paired symbols
      ("p" er/mark-outside-pairs)
      ("P" er/mark-inside-pairs)
      ("q" er/mark-outside-quotes)
      ("Q" er/mark-inside-quotes)
      ("t" er/mark-outer-tag)
      ("T" er/mark-inner-tag)

      ;; Syntactic constructs
      ("l" (lambda ()
             "Select current line"
             (interactive)
             (if (and (region-active-p)
                      (not (equal (point) (mark))))
                 (progn
                   (move-end-of-line nil)
                   (forward-char))
               (progn
                 (move-beginning-of-line nil)
                 (set-mark-command nil)
                 (move-end-of-line nil)
                 (forward-char)
                 (setq deactivate-mark nil)))))

      ;; Semantic constructs
      ("d" er/mark-defun)
      ("s" er/mark-symbol)

      ;; `expand-region'
      ("C-SPC" er/expand-region)
      ("x" er/expand-region)
      ("c" er/contract-region)

      ;; Actions
      (";" (lambda ()
      	     (interactive)
      	     (when (region-active-p)
      	       (comment-or-uncomment-region (region-beginning)
      					    (region-end)))
      	     (setq deactivate-mark nil)))
      ("=" indent-region))

    (global-set-key (kbd "C-SPC") 'hydra-mark/body)))

;; (use-package helm
;;   :config
;;   (global-set-key (kbd "M-x") 'helm-M-x))

(use-package avy
  :config
  (global-set-key (kbd "C-:") 'avy-goto-char))
