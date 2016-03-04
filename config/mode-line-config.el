;; Set up mode-line-cleaner
(require 'mode-line-cleaner)

;; Helper functions
(defun mode-line-split (left right)
  "Split `mode-line' into left aligned and right aligned parts."
  (let* ((available-width (- (window-width) (length left) 2)))
    (format (format " %%s %%%ds " available-width) left right)))

;; Set `mode-line' format
(setq-default mode-line-format
	      '((:eval (mode-line-split
			(concat
			 (propertize mode-name
				     'face '(:foreground "#F92672"))
			 " "
			 (propertize (buffer-name)
				     'face '(:foreground "#E6DB74"))
                         (when (and (fboundp 'vc-git-branches)
                                    (car (vc-git-branches)))
                           (concat " "
                                   (propertize (car (vc-git-branches))
                                               'face '(:foreground "#A6E22E"))))
                         (when (and (or (eq major-mode 'python-mode)
                                        (eq major-mode 'hy-mode))
                                    (boundp 'pyvenv-virtual-env))
                           (concat " "
                                   (propertize pyvenv-virtual-env-name
                                               'face '(:foreground "#66D9EF")))))
			(propertize (format-mode-line minor-mode-alist)
				    'face '(:foreground "#75715E"))))))

;; Provide module
(provide 'mode-line-config)
