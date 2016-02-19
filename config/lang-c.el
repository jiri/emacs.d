;; Style config
(setq-default c-default-style "k&r"
              c-basic-offset 4)

;; Enable appropriate minor modes
(use-package c-eldoc
  :config
  (add-hook 'c-mode-common-hook 'c-turn-on-eldoc-mode))

(use-package smartparens
  :config
  (add-hook 'c-mode-common-hook 'smartparens-mode))

(use-package company
  :config
  (progn
    (use-package yasnippet)
    (add-hook 'c-mode-common-hook (lambda ()
                                    (set (make-local-variable 'company-backends)
                                         '((company-yasnippet)))))))

;; Provide module
(provide 'lang-c)
