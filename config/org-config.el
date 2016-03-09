;; Show all headlines when a document is opened
(setq org-startup-folded 'content)

;; Better source code editing
(setq org-src-fontify-natively t)
(setq org-src-window-setup 'current-window)

;; Improve keybindings
(define-key org-mode-map (kbd "C-<return>") 'org-insert-heading)
(define-key org-mode-map (kbd "M-<return>") 'org-insert-heading-respect-content)

(define-key org-mode-map (kbd "C-<left>") 'org-metaleft)
(define-key org-mode-map (kbd "C-<right>") 'org-metaright)
(define-key org-mode-map (kbd "C-S-<left>") 'org-shiftmetaleft)
(define-key org-mode-map (kbd "C-S-<right>") 'org-shiftmetaright)

(define-key org-mode-map (kbd "M-<left>") 'left-word)
(define-key org-mode-map (kbd "M-<right>") 'right-word)
(define-key org-mode-map (kbd "M-S-<left>") 'org-shiftcontrolleft)
(define-key org-mode-map (kbd "M-S-<right>") 'org-shiftcontrolright)

;; Make clocks persistent
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

;; Czech support for LaTeX exporting
(push '("czech" "babel" nil) org-latex-default-packages-alist)

;; Remove annoying boxes in agenda view
(dolist (f '(org-agenda-structure
             org-agenda-date))
  (set-face-attribute f nil :box nil))

;; Provide module
(provide 'org-config)
