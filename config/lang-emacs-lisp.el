;; Enable appropriate minor modes
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)

;; Rename major modes
(mode-line-clean 'lisp-interaction-mode "λ")
(mode-line-clean 'emacs-lisp-mode "λ")

;; Provide module
(provide 'lang-emacs-lisp)
