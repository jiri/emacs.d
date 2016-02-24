;; Set up virtualenv
(use-package virtualenvwrapper
  :init
  (setq venv-location "~/.virtualenvs")
  (setenv "WORKON_HOME" "~/.virtualenvs"))

;; Set up Elpy
(use-package elpy
  :after python
  :init
  (setq elpy-rpc-backend "jedi")
  :config
  (progn
    (elpy-enable)

    ;; Disable shell completion due to a probable bug with Emacs 25
    ;; TODO - Revise this
    (push "python" python-shell-completion-native-disabled-interpreters)

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
