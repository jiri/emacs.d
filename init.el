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

;; Various keybindings
(global-set-key (kbd "C-x k") (lambda () (interactive) (kill-buffer (buffer-name))))
(global-set-key (kbd "M-[") 'previous-buffer)
(global-set-key (kbd "M-]") 'next-buffer)

;; Better evaluation
(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (end-of-defun)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(global-set-key (kbd "C-x C-e") 'eval-defun)
(global-set-key (kbd "C-c C-e") 'eval-and-replace)

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

;; Cleaner mode list
(defvar mode-line-cleaner-alist
  `(;(auto-complete-mode . " α")
    ;(yas/minor-mode . " υ")
    (paredit-mode . nil)
    (company-mode . nil)
    ;(eldoc-mode . "")
    ;(abbrev-mode . "")
    ;; Major modes
    ;(lisp-interaction-mode . "λ")
    ;(hi-lock-mode . "")
    ;(python-mode . "Py")
    ;(emacs-lisp-mode . "EL")
    ;(nxhtml-mode . "nx")
    (haskell-mode . ">>=")
    ))

(defun clean-mode-line ()
  (interactive)
  (loop for cleaner in mode-line-cleaner-alist
        do (let* ((mode (car cleaner))
                 (mode-str (cdr cleaner))
                 (old-mode-str (cdr (assq mode minor-mode-alist))))
             (when old-mode-str
                 (setcar old-mode-str mode-str))
               ;; major mode
             (when (eq mode major-mode)
               (setq mode-name mode-str)))))

(add-hook 'after-change-major-mode-hook 'clean-mode-line)

;; Disable all keybindings
; (use-global-map (make-sparse-keymap))

;; Custom modal editing
;(defvar navigation-mode-map (make-sparse-keymap))

;(define-key navigation-mode-map (kbd "h") 'backward-char)
;(define-key navigation-mode-map (kbd "j") 'next-line)
;(define-key navigation-mode-map (kbd "k") 'previous-line)
;(define-key navigation-mode-map (kbd "l") 'forward-char)
;(define-key navigation-mode-map (kbd "M-x") 'execute-extended-command)
;(define-key navigation-mode-map (kbd "i") 'insert-mode)

;; (define-minor-mode navigation-mode
;;   "Navigation mode" nil "<N>"
;;   navigation-mode-map
;;   (setq-local cursor-type
;;               (if navigation-mode
;;                   'box
;; 		(default-value 'cursor-type))))

;; (defvar insert-mode-map (make-sparse-keymap))

;; (define-key insert-mode-map [t] #'self-insert-command)
;; (define-key insert-mode-map (kbd "<escape>") 'navigation-mode)

;; (define-minor-mode insert-mode
;;   "Insert mode" nil "<I>"
;;   insert-mode-map
;;   (setq-local cursor-type
;;               (if insert-mode
;;                   'bar
;;                 (default-value 'cursor-type))))

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

;; Dired mode
(add-hook 'dired-mode-hook (lambda ()
			     (hl-line-mode)
			     (setq-local cursor-type nil)))

(with-eval-after-load "dired"
  (define-key dired-mode-map (kbd "k") 'dired-previous-line)
  (define-key dired-mode-map (kbd "j") 'dired-next-line)
  (define-key dired-mode-map (kbd "J") 'dired-jump))

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
