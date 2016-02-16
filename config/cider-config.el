;; Cider
(setq cider-repl-history-file "~/.cider-history")
(setq cider-repl-pop-to-buffer-on-connect t)
(setq cider-show-error-buffer t)
(setq cider-auto-select-error-buffer t)

;; Enable appropriate minor modes
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-repl-mode-hook 'enable-paredit-mode)

;; Improve keybindings
(define-key cider-repl-mode-map (kbd "<up>") 'cider-repl-previous-input)
(define-key cider-repl-mode-map (kbd "<down>") 'cider-repl-next-input)

(define-key cider-repl-mode-map (kbd "C-c C-k") 'cider-repl-clear-buffer)
(define-key cider-repl-mode-map (kbd "C-c C-c") 'kill-whole-line)

;; Rename minor modes
(mode-line-clean 'cider-mode "cider")

;; Provide module
(provide 'cider-config)
