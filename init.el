;; Set up modular configuration
(push (expand-file-name "config" user-emacs-directory) load-path)

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
                      which-key
		      yasnippet
                      expand-region
                      browse-kill-ring
                      org
                      c-eldoc

                      smartparens

                      ;; Python
                      elpy

		      ;; Clojure
		      clojure-mode
		      clojure-mode-extra-font-locking
		      cider
		      rainbow-delimiters))

(defun installed-from-archive-p (pkg)
  (assq pkg package-alist))

(dolist (p my-packages)
  (when (not (installed-from-archive-p p))
    (package-install (cadr (assq p package-archive-contents)))))

;; Theme
(require 'appearance)

;; OSX specific config
(setq ns-pop-up-frames nil)
(global-set-key (kbd "s-t") 'ignore)

;; Start server if it's not already started
(require 'server)
(unless (server-running-p)
  (server-start))

;; Set up exec-path to include homebrew packages
(push "/usr/local/bin/" exec-path)

;; Fix `shell-command-to-string'
(defun strip-trailing-newline (str)
  (replace-regexp-in-string "\n*$" "" str))

(advice-add 'shell-command-to-string
            :filter-return 'strip-trailing-newline)

;; Use bash instead of the default shell to make stuff consistent
(setq-default shell-file-name "bash")

;; Emacs pls
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message "sindriava")
(setq ad-redefinition-action 'accept)

(defalias 'yes-or-no-p 'y-or-n-p)
(setq confirm-nonexistent-file-or-buffer nil)

;; Sensitive scrolling
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq scroll-step 1)

;; Whitespace
(setq-default indent-tabs-mode nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Make shell scripts executable on save
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; Region settings
(delete-selection-mode)

(setq expand-region-fast-keys-enabled nil)

(global-set-key (kbd "C-SPC") (lambda ()
                                (interactive)
                                (if (region-active-p)
                                    (er/expand-region 1)
                                  (set-mark (point)))))
(global-set-key (kbd "C-S-SPC") 'er/contract-region)

;; Search
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)

;; Killing & Yanking
(global-set-key (kbd "C-w") 'kill-ring-save)
(global-set-key (kbd "M-w") 'kill-region)

(global-set-key (kbd "M-y") 'browse-kill-ring)

;; Buffer menu
(add-hook 'buffer-menu-mode-hook (lambda ()
                                   (setq-local cursor-type nil)
                                   (hl-line-mode)))

;; Buffer cycling
(defun sindriava/cycle-until-viable (action)
  (let ((bread-crumb (buffer-name)))
    (funcall action)
    (while
	(and
         (not (equal bread-crumb (buffer-name)))
         (not (equal (buffer-name) "*scratch*"))
         (or (string-match-p "^\*" (buffer-name))))
      (funcall action))))

(global-set-key (kbd "M-[") (lambda ()
                              (interactive)
                              (sindriava/cycle-until-viable 'next-buffer)))
(global-set-key (kbd "M-]") (lambda ()
                              (interactive)
                              (sindriava/cycle-until-viable 'previous-buffer)))

;; Rebind some defaults
(global-set-key (kbd "<escape>") 'keyboard-quit)
(global-set-key (kbd "s-q") 'delete-frame)

(global-set-key (kbd "C-a") (lambda ()
			      (interactive)
			      (let ((old-point (point)))
				(back-to-indentation)
				(when (= (point) old-point)
				  (beginning-of-line)))))

(global-set-key (kbd "C-x k") (lambda ()
				(interactive)
				(let ((buffer (buffer-name)))
				  (if (equal buffer "*scratch*")
				      (message "Cannot kill scratch buffer.")
				    (progn
                                      (sindriava/cycle-until-viable 'next-buffer)
                                      (kill-buffer buffer))))))

;; Window management
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
         (next-win-buffer (window-buffer (next-window)))
         (this-win-edges (window-edges (selected-window)))
         (next-win-edges (window-edges (next-window)))
         (this-win-2nd (not (and (<= (car this-win-edges)
                     (car next-win-edges))
                     (<= (cadr this-win-edges)
                     (cadr next-win-edges)))))
         (splitter
          (if (= (car this-win-edges)
             (car (window-edges (next-window))))
          'split-window-horizontally
        'split-window-vertically)))
    (delete-other-windows)
    (let ((first-win (selected-window)))
      (funcall splitter)
      (if this-win-2nd (other-window 1))
      (set-window-buffer (selected-window) this-win-buffer)
      (set-window-buffer (next-window) next-win-buffer)
      (select-window first-win)
      (if this-win-2nd (other-window 1))))))

(global-set-key (kbd "C-o") 'other-window)
(global-set-key (kbd "C-x t") 'toggle-window-split)

(defadvice quit-window (before quit-window-always-kill)
  "Make sure that live windows stay quit"
  (ad-set-arg 0 t))
(ad-activate 'quit-window)

;; Prettier cursor
(setq-default cursor-type 'bar)
(set-cursor-color "#F92672")

;; Eshell
(global-set-key (kbd "C-c e") 'eshell)

(eval-after-load 'eshell '(require 'eshell-config))

;; Which key
(which-key-mode)

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)

(eval-after-load 'magit '(require 'magit-config))

;; Yasnippet
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"))

(yas-global-mode)

;; Paredit
(autoload 'enable-paredit-mode "paredit" t)

(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)

(add-hook 'paredit-mode-hook 'rainbow-delimiters-mode)


;; Org mode
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c b") 'org-iswitchb)
(global-set-key (kbd "C-c l") 'org-store-link)

(eval-after-load 'org '(require 'org-config))

;; Emacs Lisp
(require 'lang-emacs-lisp)

;; Python
(require 'lang-python)

;; Haskell
(require 'lang-haskell)

;; C
(setq-default c-default-style "k&r"
              c-basic-offset 4)

(add-hook 'c-mode-common-hook (lambda ()
                                (c-turn-on-eldoc-mode)
                                (set (make-local-variable 'company-backends)
                                     '((company-yasnippet)))))
(add-hook 'c-mode-common-hook 'smartparens-mode)

;; Clojure
(require 'lang-clojure)

;; Completion
(add-hook 'prog-mode-hook 'company-mode)

(setq company-idle-delay 0.25)

;; Prettify symbols
(global-prettify-symbols-mode)

;; Dired
(eval-after-load 'dired '(require 'dired-config))

;; Backups & custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror 'nomessage)

(setq create-lockfiles nil)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(push 'select-frame-set-input-focus after-make-frame-functions)

;; Run on full power
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
