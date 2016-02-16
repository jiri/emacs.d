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
				     'face '(:foreground "#E6DB74")))
			(propertize (format-mode-line minor-mode-alist)
				    'face '(:foreground "#75715E"))))))

;; Provide module
(provide 'mode-line-config)
