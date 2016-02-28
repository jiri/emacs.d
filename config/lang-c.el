(use-package cedet
  :config
  (progn
    ;; Completion
    (require 'semantic)
    (global-semanticdb-minor-mode)
    (global-semantic-idle-scheduler-mode)

    (add-hook 'c-mode-common-hook 'semantic-mode)

    (with-eval-after-load 'company
      (defun sindriava/indent-or-complete ()
        "Indent if appropriate, start completion otherwise."
        (interactive)
        (if (looking-at "\\_>")
            (company-complete-common)
          (indent-according-to-mode)))

      (define-key c-mode-map (kbd "TAB") 'sindriava/indent-or-complete)
      (define-key c++-mode-map (kbd "TAB") 'sindriava/indent-or-complete))

    ;; Compilation
    (defun sindriava/compile ()
      (interactive)
      (setq-local compilation-read-command nil)
      (call-interactively 'compile))

    (define-key c-mode-map (kbd "C-c C-c") 'sindriava/compile)))

;; Provide module
(provide 'lang-c)
