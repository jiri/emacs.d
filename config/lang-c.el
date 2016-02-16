;; Style config
(setq-default c-default-style "k&r"
              c-basic-offset 4)

;; Enable appropriate minor modes
(add-hook 'c-mode-common-hook 'c-turn-on-eldoc-mode)
(add-hook 'c-mode-common-hook 'smartparens-mode)
(add-hook 'c-mode-common-hook (lambda ()
                                (set (make-local-variable 'company-backends)
                                     '((company-yasnippet)))))

;; Provide module
(provide 'lang-c)
