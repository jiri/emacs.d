;; Better source code editing
(setq org-src-fontify-natively t)
(setq org-src-window-setup 'current-window)

;; Improve keybindings
(define-key org-mode-map (kbd "C-RET") 'org-insert-heading)
(define-key org-mode-map (kbd "M-RET") 'org-insert-heading-respect-content)

;; Make clocks persistent
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

;; Provide module
(provide 'org-config)
