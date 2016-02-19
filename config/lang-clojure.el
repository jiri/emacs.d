;; Set up Clojure
(use-package clojure-mode
  :defer t
  :config
  (progn
    (use-package clojure-mode-extra-font-locking)
    (use-package cider
      :init
      (setq cider-repl-history-file "~/.cider-history"
            cider-repl-pop-to-buffer-on-connect t
            cider-show-error-buffer t
            cider-auto-select-error-buffer t)
      :config
      (progn
        ;; Enable appropriate minor modes
        (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
        (add-hook 'cider-repl-mode-hook 'enable-paredit-mode)

        ;; Improve keybindings
        (define-key cider-repl-mode-map (kbd "<up>") 'cider-repl-previous-input)
        (define-key cider-repl-mode-map (kbd "<down>") 'cider-repl-next-input)

        (define-key cider-repl-mode-map (kbd "C-c C-k") 'cider-repl-clear-buffer)
        (define-key cider-repl-mode-map (kbd "C-c C-c") 'kill-whole-line)

        ;; Rename minor modes
        (mode-line-clean 'cider-mode "cider")))
    ;; Enable appropriate minor modes
    (add-hook 'clojure-mode-hook 'paredit-mode)))

;; Provide module
(provide 'lang-clojure)
