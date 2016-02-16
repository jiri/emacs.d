;; Open Magit in fullscreen
(defadvice magit-status (around magit-fullscreen activate)
  "Open `magit-status' full-screen"
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

;; Provide module
(provide 'magit-config)
