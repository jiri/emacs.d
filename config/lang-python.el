;; Set up virtualenv
(use-package virtualenvwrapper
  :init
  (setq venv-location '("~/Code/Python/virtualenvs")))

;; Set up Elpy
(use-package elpy
  :after python
  :init
  (setq elpy-rpc-python-command "/usr/local/bin/python3"
        elpy-rpc-backend "jedi")
  :config
  (progn
    (elpy-enable)
    (elpy-use-cpython "/usr/local/bin/python3")

    ;; Disable shell completion due to a probable bug with Emacs 25
    ;; TODO - Revise this
    (push "python3" python-shell-completion-native-disabled-interpreters)

    ;; Disable unused `elpy' modules
    (remove-hook 'elpy-modules 'elpy-module-flymake)
    (remove-hook 'elpy-modules 'elpy-module-highlight-indentation)

    ;; Enable appropriate minor modes
    (add-hook 'python-mode-hook 'smartparens-mode)

    ;; Clean up mode names
    (mode-line-clean 'python-mode "Py")
    (mode-line-clean 'elpy-mode)))

;; Set up Hy
(use-package hy-mode
  :config
  (add-hook 'hy-mode-hook 'paredit-mode))

;; Provide module
(provide 'lang-python)
