;; Set up Haskell
(use-package haskell-mode
  :defer t
  :init
  (setq haskell-process-type 'stack-ghci

        haskell-process-suggest-remove-import-lines t
        haskell-process-auto-import-loaded-modules t
        haskell-process-log t)
  :config
  (progn
    ;; Enable appropriate minor modes
    (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
    (add-hook 'haskell-mode-hook 'smartparens-mode)

    ;; Set up completion
    (use-package ghc
      :config
      (add-hook 'haskell-mode-hook (lambda () (ghc-init))))

    (use-package company-ghc
      :config
      (add-to-list 'company-backend 'company-ghc))

    ;; Set up automatic indentation
    (use-package hindent
      :config
      (add-hook 'haskell-mode-hook 'hindent-mode))

    ;; Rename minor modes
    (mode-line-clean 'interactive-haskell-mode "ρ")

    ;; Rename major modes
    (mode-line-clean 'haskell-mode ">>=")
    (mode-line-clean 'haskell-interactive-mode "Ghci")
    (mode-line-clean 'haskell-cabal-mode "Cabal")))

;; Provide module
(provide 'lang-haskell)
