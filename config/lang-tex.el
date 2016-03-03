(use-package tex
  :ensure auctex
  :defer t
  :init
  (setq TeX-auto-save t
        TeX-parse-self t)
  :config
  (progn
    (add-hook 'tex-mode-hook 'smartparens-mode)
    (add-hook 'latex-mode-hook 'smartparens-mode)

    ;; Completion
    (use-package company-auctex
      :config
      (progn
        (company-auctex-init)
        (add-hook 'tex-mode-hook 'company-mode)
        (add-hook 'latex-mode-hook 'company-mode)))

    ;; Preview pane
    (use-package latex-preview-pane
      :config
      (progn
        (add-hook 'latex-mode-hook 'latex-preview-pane-enable)
        (add-hook 'tex-mode-hook 'latex-preview-pane-enable)))))

;; Provide module
(provide 'lang-tex)
