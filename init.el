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
    (mode-line-clean 'golden-ratio-mode "φ")

    (advice-add 'select-window :after (lambda (&rest args) (golden-ratio)))))

;; OSX specific config
(setq ns-pop-up-frames nil)
(global-set-key (kbd "s-t") 'ignore)

;; Start server if it's not already started
(require 'server)
(unless (server-running-p)
  (server-start))

;; `PATH' configuration
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

;; Fix `shell-command-to-string'
(defun strip-trailing-newline (str)
  (replace-regexp-in-string "\n*\\'" "" str))

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

;; Configure `re-builder'
(with-eval-after-load 're-builder
  (setq reb-re-syntax 'string))

;; Keybindings
(require 'bindings)

;; Shackle
(use-package shackle
  :init
  (setq shackle-default-rule '(:select t))
  :config
  (shackle-mode))

;; Helm
(use-package helm
  :demand
  :preface
  (setq initial-scratch-message
      (concat ";; This buffer is for text that is not saved, and for Lisp evaluation.\n"
              ";; To create a file, visit it with \\[helm-find-files] and enter text in its buffer.\n\n"))
  :bind
  (("C-x C-f" . helm-find-files)
   ("C-x b"   . helm-mini)
   ("M-x"     . helm-M-x)
   ("M-y"     . helm-show-kill-ring))
  :init
  (setq helm-split-window-in-side-p t
        helm-M-x-fuzzy-match t
        helm-semantic-fuzzy-match t
        helm-imenu-fuzzy-match t
        helm-apropos-fuzzy-match t
        helm-locate-fuzzy-match t
        helm-locate-command "mdfind -name %s %s")
  :config
  (progn
    ;; Set up `helm-command-prefix'
    (require 'helm-config)

    (global-unset-key (kbd "C-x c"))
    (global-set-key (kbd "C-c h") 'helm-command-prefix)

    ;; Override some default keybindings
    (global-set-key (kbd "C-c h o") 'helm-occur)
    (global-set-key (kbd "C-c h SPC") 'helm-all-mark-rings)
    (global-set-key (kbd "C-c h x") 'helm-register)
    (global-set-key (kbd "C-c h g") 'helm-google-suggest)

    ;; Use `curl' when possible
    (when (executable-find "curl")
      (setq helm-google-suggest-use-curl-p t))

    ;; Keep helm at the bottom
    (with-eval-after-load 'shackle
      (push '("\\`\\*helm.*?\\*\\'" :regexp t :align t :ratio 0.4) shackle-rules))

    ;; Swap `C-z' and `<tab>'
    (define-key helm-map (kbd "TAB") 'helm-execute-persistent-action)
    (define-key helm-map (kbd "C-z") 'helm-select-action)

    ;; Prevent `golden-ratio' from interfering with `helm'
    (with-eval-after-load 'golden-ratio
      (defun sindriava/helm-alive-p ()
        (if (boundp 'helm-alive-p)
            (symbol-value 'helm-alive-p)))

      (add-to-list 'golden-ratio-inhibit-functions 'sindriava/helm-alive-p))

    ;; Hide the `mode-line' in `helm' buffers
    (defun helm-display-mode-line (source &optional force)
      (setq mode-line-format nil))

    ;; Turn on Helm globally
    (helm-mode 1)
    (mode-line-clean 'helm-mode)))

;; Which key
(use-package which-key
  :config
  (progn
    (which-key-mode)
    (mode-line-clean 'which-key-mode)))

;; Music
(use-package simple-mpc
  :config
  (progn
    ;; Helper functions
    (defun sindriava/note ()
      (propertize "♬" 'face '(:foreground "#F92672")))

    (defun sindriava/song-title (offset)
      (let* ((pos (simple-mpc-get-current-playlist-position)))
        (or (when (> pos 0)
              (nth-value (+ pos offset -1)
                         (split-string (shell-command-to-string "mpc playlist") "\n")))
            "")))

    ;; Define a hydra for controlling `simple-mpc'
    (defhydra hydra-mpc
      (:body-pre (setq hydra-is-helpful t)
       :post (setq hydra-is-helpful nil)
       :hint nil)
      (concat "\n"
              "   ^_t_^       %s(sindriava/song-title -1)" "\n"
              " _p_   _n_   %s(sindriava/note) %s(sindriava/song-title 0)" "\n"
              "   ^_s_^       %s(sindriava/song-title 1)" "\n")
      ("n" simple-mpc-next)
      ("p" simple-mpc-prev)
      ("c" simple-mpc-view-current-playlist :color blue)
      ("s" simple-mpc-query :color blue)
      ("t" simple-mpc-toggle)
      ("SPC" simple-mpc-toggle :color blue)

      ("q" nil :color blue))

    (global-set-key (kbd "C-c m") 'hydra-mpc/body)

    ;; Hide `simple-mpc' modes
    (mode-line-clean 'simple-mpc-current-playlist-mode)))

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

    ;; Set up `shackle' for `magit'
    (with-eval-after-load 'shackle
      (push '("\\`\\*magit-diff: .*?\\'" :regexp t :noselect t) shackle-rules))))

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
    (add-hook 'eval-expression-minibuffer-setup-hook 'enable-paredit-mode)
    (add-hook 'inferior-lisp-mode-hook 'enable-paredit-mode)

    (use-package rainbow-delimiters
      :config
      (add-hook 'paredit-mode-hook 'rainbow-delimiters-mode))

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

(use-package fish-mode)

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

;; Hydra
(use-package hydra
  :init
  (setq hydra-is-helpful nil))

;; Avy
(use-package avy
  :bind (("C-;" . avy-goto-char)
         ("C-:" . avy-goto-word-1))
  :config
  (with-eval-after-load 'golden-ratio
    ;; Resize windows after Avy jumps between them
    (defun avy-golden-ratio (&rest args)
      (golden-ratio))

    (advice-add 'avy-action-goto :after 'avy-golden-ratio)))

;; Anchored transpose
(use-package anchored-transpose
  :bind ("C-t" . anchored-transpose))

;; Dired
(with-eval-after-load 'dired
  (require 'dired-config))

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
