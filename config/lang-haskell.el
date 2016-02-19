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

    ;; Rename major modes
    (mode-line-clean 'haskell-mode ">>=")
    (mode-line-clean 'haskell-interactive-mode "Ghci")
    (mode-line-clean 'haskell-cabal-mode "Cabal")))

;; Provide module
(provide 'lang-haskell)
