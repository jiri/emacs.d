;; Initialize `package'
(require 'package)

(package-initialize nil)

;; Add MELPA to `package-archives'
(unless (assoc "melpa" package-archives)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.org/packages/") t))

(unless package-archive-contents
  (package-refresh-contents))

;; Set up `use-package'
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(setq use-package-always-ensure t)
(setq use-package-verbose t)

(setq load-prefer-newer t)

;; Set up modular configuration
(push (expand-file-name "config" user-emacs-directory) load-path)

;; Temporary workaround for problems with `package.el' and `use-package'
;; TODO - Revisit this
(defun package-from-archive (f &rest args)
  "Make an exception for `org' when checking if package is installed"
  (if (equal (car args) 'org)
      (assq 'org package-alist)
    (apply f args)))

(advice-add 'package-installed-p :around 'package-from-archive)

;; Theme
(require 'appearance)

;; Golden ratio
(use-package golden-ratio
  :config
  (progn
    (golden-ratio-mode)
    (mode-line-clean 'golden-ratio-mode "φ")))

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

;; Sensitive scrolling
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq scroll-step 1)

;; Whitespace
(setq-default indent-tabs-mode nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Make shell scripts executable on save
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; Keybindings
(require 'bindings)

;; Eshell
(use-package eshell
  :bind ("C-c e" . eshell)
  :config
  (require 'eshell-config))

;; Which key
(use-package which-key
  :config
  (progn
    (which-key-mode)
    (mode-line-clean 'which-key-mode)))

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

    (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)))

;; Git modes
(use-package gitconfig-mode
  :defer t)
(use-package gitattributes-mode
  :defer t)
(use-package gitignore-mode
  :defer t)

;; Gist
(use-package gist
  :bind ("C-c p" . gist-region-or-buffer))

;; Yasnippet
(use-package yasnippet
  :init
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  :config
  (progn
    (add-hook 'prog-mode-hook 'yas-minor-mode)
    (mode-line-clean 'yas-minor-mode "y")))

;; Smartparens
(use-package smartparens
  :config
  (mode-line-clean 'smartparens-mode "σ"))

;; Paredit
(use-package paredit
  :config
  (progn
    (use-package rainbow-delimiters)
    (add-hook 'eval-expression-minibuffer-setup-hook 'enable-paredit-mode)
    (add-hook 'paredit-mode-hook 'rainbow-delimiters-mode)

    (mode-line-clean 'paredit-mode "π")))

;; Org mode
(use-package org
  :defer t
  :pin gnu
  :bind
  (("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   ("C-c b" . org-iswitchb)
   ("C-c l" . org-store-link))
  :config
  (require 'org-config))

;; Programming languages
(require 'lang-emacs-lisp)
(require 'lang-python)
(require 'lang-haskell)
(require 'lang-c)
(require 'lang-clojure)

;; Completion
(use-package company
  :init
  (setq company-idle-delay 0.25)
  :config
  (progn
   (add-hook 'prog-mode-hook 'company-mode)
   (mode-line-clean 'company-mode)))

;; Prettify symbols
(global-prettify-symbols-mode)

;; Avy
(use-package avy
  :bind (("C-;" . avy-goto-char)
         ("C-:" . avy-goto-word-1))
  :config
  (use-package golden-ratio
    :ensure nil
    :config
    (progn
      ;; Resize windows after Avy jumps between them
      (defun avy-golden-ratio (&rest args)
        (golden-ratio))

      (advice-add 'avy-action-goto :after 'avy-golden-ratio))))

;; Anchored transpose
(use-package anchored-transpose
  :bind ("C-t" . anchored-transpose))

;; Dired
(eval-after-load 'dired '(require 'dired-config))

;; Clean up miscellaneous minor modes
(mode-line-clean 'auto-revert-mode "α")
(mode-line-clean 'isearch-mode)

;; Backups & custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror 'nomessage)

(setq create-lockfiles nil)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Grab focus when new frame is created
(push 'select-frame-set-input-focus after-make-frame-functions)

;; Run on full power
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
