(require 'package)

(package-initialize nil)

(unless (assoc "melpa" package-archives)
  (add-to-list 'package-archives
	       '("melpa" . "https://melpa.org/packages/") t))

(unless package-archive-contents
  (package-refresh-contents))

;; Set up `use-package'
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(setq use-package-always-ensure t)

;; Set up paredit
(use-package paredit
  :config
  (progn
    (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
    (add-hook 'inferior-lisp-mode-hook 'enable-paredit-mode)

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

(defun font-available-p (font)
  "Check if FONT is available on the system"
  (member font (font-family-list)))

(when (font-available-p "Input")
  (set-face-attribute 'default nil :font "Input-13"))

;; Disable GUI cruft
(when (fboundp 'tool-bar-mode) (tool-bar-mode 0))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode 0))

(setq frame-title-format nil)
(setq ring-bell-function 'ignore)

;; Disable startup screen
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message "sindriava")

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

;; Rebind some defaults
(defun sindriava/beginning-of-line ()
  "Move to first non-whitespace character on `C-a' first."
  (interactive)
  (let ((old-point (point)))
    (back-to-indentation)
    (when (= (point) old-point)
      (beginning-of-line))))

(global-set-key (kbd "C-a") 'sindriava/beginning-of-line)

;; Plugins
(use-package expand-region
  :config
  (progn
    (global-set-key (kbd "C-SPC") 'er/expand-region)
    (global-set-key (kbd "C-S-SPC") 'er/contract-region)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (magit use-package rainbow-delimiters paredit monokai-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
