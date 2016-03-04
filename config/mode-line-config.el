;; Set up mode-line-cleaner
(require 'mode-line-cleaner)

;; Helper functions
(defun mode-line-split (left right)
  "Split `mode-line' into left aligned and right aligned parts."
  (let* ((available-width (- (window-width) (length left) 2)))
    (format (format " %%s %%%ds " available-width) left right)))

(defun mode-line/mode-name ()
  "The name of the current major mode."
  (propertize mode-name
              'face '(:foreground "#F92672")))

(defun mode-line/buffer-name ()
  "The name of the current buffer."
  (concat " "
          (propertize (buffer-name)
                      'face '(:foreground "#E6DB74"))))

(defun mode-line/git-branch ()
  "The Git branch of the current file (if there is any)."
  (when (and (fboundp 'vc-git-branches)
             (car (vc-git-branches)))
    (concat " "
            (propertize (car (vc-git-branches))
                        'face '(:foreground "#A6E22E")))))

(defun mode-line/virtualenv ()
  "The current Python virtualenv if virtualenv is active."
  (when (and (or (eq major-mode 'python-mode)
                 (eq major-mode 'hy-mode))
             (boundp 'pyvenv-virtual-env))
    (concat " "
            (propertize pyvenv-virtual-env-name
                        'face '(:foreground "#66D9EF")))))

(defun mode-line/minor-modes ()
  "Render `minor-mode-alist'"
  (propertize (format-mode-line minor-mode-alist)
              'face '(:foreground "#75715E")))

;; Set `mode-line' format
(setq-default mode-line-format
	      '((:eval (mode-line-split
			(concat (mode-line/mode-name)
                                (mode-line/buffer-name)
                                (mode-line/git-branch)
                                (mode-line/virtualenv))
			(mode-line/minor-modes)))))

;; Provide module
(provide 'mode-line-config)
