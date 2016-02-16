;; Buffer cycling
(defun sindriava/cycle-until-viable (action)
  "Cycle buffers while the current buffer is disabled."
  (let ((bread-crumb (buffer-name)))
    (funcall action)
    (while
	(and
         (not (equal bread-crumb (buffer-name)))
         (not (equal (buffer-name) "*scratch*"))
         (or (string-match-p "^\*" (buffer-name))))
      (funcall action))))

;; Bind keys
(global-set-key (kbd "M-[") (lambda ()
                              (interactive)
                              (sindriava/cycle-until-viable 'next-buffer)))
(global-set-key (kbd "M-]") (lambda ()
                              (interactive)
                              (sindriava/cycle-until-viable 'previous-buffer)))

;; Provide module
(provide 'bindings-buffer)
