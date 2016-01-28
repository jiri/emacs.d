;; Package setup
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)

;; Automatically install packages
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(magit
                      paredit
                      company
		      haskell-mode
                      monokai-theme
                      which-key))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Theme
(setq default-frame-alist '((width  . 80)
			    (height . 25)
			    (font   . "Input-16")))

(setq monokai-use-variable-pitch nil)
(load-theme 'monokai t)

;; Hide redundant UI elements
(tool-bar-mode 0)
(scroll-bar-mode 0)
(setq frame-title-format nil)
(setq ring-bell-function 'ignore)
(setq-default truncate-lines 1)

;; Set up exec-path to include homebrew packages
(setq exec-path (cons "/usr/local/bin/" exec-path))

;; Emacs pls
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message "sindriava")

(defalias 'yes-or-no-p 'y-or-n-p)

;; Sensitive scrolling
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq scroll-step 1)

;; Whitespace
(setq indent-tabs-mode nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Buffer cycling
(defvar sindriava/cycle-exclude-buffers
  '("*Messages*"
    "*Completions*"
    "*Backtrace*"))

(defun sindriava/cycle-until-viable (action)
  (let ((bread-crumb (buffer-name)))
    (funcall action)
    (while
	(and
	 (member (buffer-name) sindriava/cycle-exclude-buffers)
	 (not (equal bread-crumb (buffer-name))))
      (funcall action))))

(global-set-key (kbd "M-[") (lambda ()
                              (interactive)
                              (sindriava/cycle-until-viable 'next-buffer)))
(global-set-key (kbd "M-]") (lambda ()
                              (interactive)
                              (sindriava/cycle-until-viable 'previous-buffer)))

;; Rebind some defaults
(global-set-key (kbd "C-a") 'back-to-indentation)

;; Various keybindings
(global-set-key (kbd "C-j") (lambda () (interactive) (join-line -1)))
(global-set-key (kbd "C-x k") (lambda ()
				(interactive)
				(if (equal (buffer-name) "*scratch*")
				    (message "Cannot kill scratch buffer.")
				  (kill-buffer (buffer-name)))))

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
    (lisp-interaction-mode    . "λ")
    (emacs-lisp-mode	      . "λ")
    (haskell-mode	      . ">>=")
    (haskell-interactive-mode . "Ghci")
    (haskell-cabal-mode       . "Cabal")))

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
		(:eval (propertize "%b"   'face '(:foreground "#E6DB74")))
		" "
		(:eval (when (fboundp 'vc-git-branches)
			 (propertize (car (vc-git-branches)) 'face '(:foreground "#A6E22E"))))))

;; Eshell
(global-set-key (kbd "C-c e") 'eshell)

(defun fish-path (path max-len)
  "Return a potentially trimmed-down version of the directory PATH, replacing
parent directories with their initial characters to try to get the character
length of PATH (sans directory slashes) down to MAX-LEN."
  (let* ((components (split-string (abbreviate-file-name path) "/"))
         (len (+ (1- (length components))
                 (cl-reduce '+ components :key 'length)))
         (str ""))
    (while (and (> len max-len)
                (cdr components))
      (setq str (concat str
                        (cond ((= 0 (length (car components))) "/")
                              ((= 1 (length (car components)))
                               (concat (car components) "/"))
                              (t
                               (if (string= "."
                                            (string (elt (car components) 0)))
                                   (concat (substring (car components) 0 2)
                                           "/")
                                 (string (elt (car components) 0) ?/)))))
            len (- len (1- (length (car components))))
            components (cdr components)))
    (concat str (cl-reduce (lambda (a b) (concat a "/" b)) components))))

(add-hook 'eshell-mode-hook
          (lambda ()
	    (define-key eshell-mode-map (kbd "C-c C-l")
	      (lambda ()
		(interactive)
		(eshell/clear-scrollback)
		(eshell-send-input nil nil t)))
	    (setq eshell-path-env (concat "/usr/local/bin:" eshell-path-env))
	    (set-face-attribute 'eshell-prompt nil
				:foreground 'unspecified
				:weight 'unspecified)))

(setq eshell-prompt-regexp "^λ [^ ]* ")
(setq eshell-prompt-function (lambda ()
                               (concat (propertize "λ " 'face '(:foreground "#F92672"))
				       (fish-path (eshell/pwd) 1)
				       " ")))

;; Which key
(which-key-mode)

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)

;; Paredit
(autoload 'enable-paredit-mode "paredit" t)

(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)

;; Org mode
(setq org-default-notes-file "~/notes.org")

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c b") 'org-iswitchb)
(global-set-key (kbd "C-c l") 'org-store-link)

;; Haskell
(require 'haskell-interactive-mode)
(require 'haskell-process)

(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

(setq haskell-process-type 'stack-ghci

      haskell-process-suggest-remove-import-lines t
      haskell-process-auto-import-loaded-modules t
      haskell-process-log t)

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
