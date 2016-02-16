;; Dired mode
(require 'dired-x)

;; Appearance
(set-face-attribute 'dired-header nil
                    :foreground "#F92672"
                    :background 'unspecified
                    :weight 'bold)

;; Make Dired more GUI-like
(add-hook 'dired-mode-hook (lambda ()
			     ;; Hide unnecessary files & info
			     (setq dired-omit-files "^\\..*$")
			     (dired-omit-mode 1)

			     (dired-hide-details-mode)

			     ;; Hide the cursor
			     (hl-line-mode)
			     (setq-local cursor-type nil)))

;; Improve keybindings
(put 'dired-find-alternate-file 'disabled nil)

(define-key dired-mode-map (kbd ".") 'dired-omit-mode)
(define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
(define-key dired-mode-map (kbd "SPC") 'dired-find-alternate-file)
(define-key dired-mode-map (kbd "DEL") (lambda ()
                                         (interactive)
                                         (find-alternate-file "..")))

;; Provide module
(provide 'dired-config)
