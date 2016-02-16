;; Setup Elpy
(elpy-enable)
(elpy-use-cpython "/usr/local/bin/python3")

(setq elpy-rpc-python-command "/usr/local/bin/python3")
(setq elpy-rpc-backend "jedi")

;; Disable shell completion due to a probable bug with Emacs 25
;; TODO - Revise this
(push "python3" python-shell-completion-native-disabled-interpreters)

;; Disable unused `elpy' modules
(remove-hook 'elpy-modules 'elpy-module-flymake)
(remove-hook 'elpy-modules 'elpy-module-highlight-indentation)

;; Enable appropriate minor modes
(add-hook 'python-mode-hook 'smartparens-mode)

;; Rename major modes
(mode-line-clean 'python-mode "Py")

;; Clean up minor modes
(mode-line-clean 'elpy-mode)

;; Provide module
(provide 'lang-python)
