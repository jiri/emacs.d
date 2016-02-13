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

                      ;; Python
                      elpy
                      jedi

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

;; Sensitive scrolling
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq scroll-step 1)

;; Whitespace
(setq-default indent-tabs-mode nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

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
				      (kill-buffer buffer)
				      (sindriava/cycle-until-viable 'next-buffer))))))

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

(global-set-key (kbd "C-x t") 'toggle-window-split)

(defadvice quit-window (before quit-window-always-kill)
  "Make sure that live windows stay quit"
  (ad-set-arg 0 t))
(ad-activate 'quit-window)

;; Prettier cursor
(setq-default cursor-type 'bar)
(set-cursor-color "#F92672")

;; Modeline
(defvar mode-line-cleaner-alist
  `(;;Minor modes
    (paredit-mode   . "π")
    (company-mode   . nil)
    (which-key-mode . nil)
    (cider-mode . "cider")
    (yas-minor-mode . "y")
    (auto-revert-mode . nil)

    ;; Major modes
    (lisp-interaction-mode    . "λ")
    (emacs-lisp-mode	      . "λ")
    (haskell-mode	      . ">>=")
    (haskell-interactive-mode . "Ghci")
    (haskell-cabal-mode       . "Cabal")))

(defun clean-mode-line ()
  (interactive)
  (dolist (cleaner mode-line-cleaner-alist)
    (let* ((mode (car cleaner))
           (mode-str (cdr cleaner))
           (old-mode-str (cdr (assq mode minor-mode-alist))))
      (when old-mode-str
        (setcar old-mode-str (concat (when mode-str " ") mode-str)))
      ;; major mode
      (when (eq mode major-mode)
        (setq mode-name mode-str)))))

(add-hook 'after-change-major-mode-hook 'clean-mode-line)

(defun mode-line-split (left right)
  (let* ((available-width (- (window-width) (length left) 2)))
    (format (format " %%s %%%ds " available-width) left right)))

(defun mode-line/git-branch ()
  (interactive)
  (when (fboundp 'vc-git-branches)
    (propertize (car (vc-git-branches))
                'face '(:foreground "#A6E22E"))))

(setq-default mode-line-format
	      '((:eval (mode-line-split
			(concat
			 (propertize mode-name
				     'face '(:foreground "#F92672"))
			 " "
			 (propertize (buffer-name)
				     'face '(:foreground "#E6DB74"))
			 " ")
			(propertize (format-mode-line minor-mode-alist)
				    'face '(:foreground "#75715E"))))))

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

(defadvice magit-status (around magit-fullscreen activate)
  "Open `magit-status' full-screen"
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(with-eval-after-load 'magit
  (define-key magit-status-mode-map (kbd "q") 'magit-quit-session))

;; Yasnippet
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"))

(yas-global-mode)

;; Paredit
(autoload 'enable-paredit-mode "paredit" t)

(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)

(add-hook 'paredit-mode-hook 'rainbow-delimiters-mode)

;; Org mode
(setq org-src-fontify-natively t)
(setq org-src-window-setup 'current-window)

(setq org-default-notes-file "~/notes.org")

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c b") 'org-iswitchb)
(global-set-key (kbd "C-c l") 'org-store-link)

;; Python
(elpy-enable)
(elpy-use-cpython "/usr/local/bin/python3")

(push "python3" python-shell-completion-native-disabled-interpreters)

(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'elpy-mode-hook (lambda ()
                            (highlight-indentation-mode -1)))

(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)

;; Haskell
(require 'haskell-interactive-mode)
(require 'haskell-process)

(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

(setq haskell-process-type 'stack-ghci

      haskell-process-suggest-remove-import-lines t
      haskell-process-auto-import-loaded-modules t
      haskell-process-log t)

;; C
(setq-default c-default-style "k&r"
              c-basic-offset 4)

(add-hook 'c-mode-common-hook (lambda ()
                                (c-turn-on-eldoc-mode)
                                (set (make-local-variable 'company-backends)
                                     '((company-yasnippet)))))

;; Clojure
(require 'clojure-mode-extra-font-locking)

(add-hook 'clojure-mode-hook 'paredit-mode)

;; CIDER
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-repl-mode-hook 'enable-paredit-mode)

(setq cider-repl-pop-to-buffer-on-connect t)
(setq cider-show-error-buffer t)
(setq cider-auto-select-error-buffer t)
(setq cider-repl-history-file "~/.cider-history")
;;(setq cider-repl-wrap-history t)

(add-hook 'cider-repl-mode-hook
	  (lambda ()
	    (define-key cider-repl-mode-map (kbd "<up>") 'cider-repl-previous-input)
	    (define-key cider-repl-mode-map (kbd "<down>") 'cider-repl-next-input)

	    (define-key cider-repl-mode-map (kbd "C-c C-k") 'cider-repl-clear-buffer)
	    (define-key cider-repl-mode-map (kbd "C-c C-c") 'kill-whole-line)))

;; Completion
(global-company-mode)

(setq company-idle-delay 0.25)

;; Prettify symbols
(global-prettify-symbols-mode)

;; Dired mode
(require 'dired-x)

(put 'dired-find-alternate-file 'disabled nil)

(add-hook 'dired-mode-hook (lambda ()
			     ;; Hide unnecessary files & info
			     (setq dired-omit-files "^\\..*$")
			     (dired-omit-mode 1)

			     (dired-hide-details-mode)

			     ;; hide the cursor
			     (hl-line-mode)
			     (setq-local cursor-type nil)))

(with-eval-after-load "dired"
  (set-face-attribute 'dired-header nil
		      :foreground "#F92672"
		      :background 'unspecified
		      :weight 'bold)

  (define-key dired-mode-map (kbd ".") 'dired-omit-mode)

  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "DEL") (lambda ()
					   (interactive)
					   (find-alternate-file ".."))))

;; Backups & custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror 'nomessage)

(setq create-lockfiles nil)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Center newly created frames on the screen
(defun sindriava/center-frame (frame)
  "Center `frame' on display."
  (set-frame-position frame
                      (/ (- (display-pixel-width)
                            (frame-pixel-width frame))
                         2)
                      (/ (- (display-pixel-height)
                            (frame-pixel-height frame))
                         2)))

(push 'sindriava/center-frame after-make-frame-functions)
