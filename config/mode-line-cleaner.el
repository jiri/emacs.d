(defvar mode-line-cleaner-alist
  `(;;Minor modes
    (company-mode . nil)
    (which-key-mode . nil)
    (yas-minor-mode . "y")
    (auto-revert-mode . nil)
    (smartparens-mode . "σ")))

(defun mode-line-clean (mode &optional str)
  "Register a name replacement `str' for `mode'."
  (push `(,mode . ,str) mode-line-cleaner-alist))

(defun mode-line-cleaner ()
  "Substitute mode names according to `mode-line-cleaner-alist'."
  (let ((cleaner-modes (delete-dups
                        (mapcar 'car mode-line-cleaner-alist))))
    (dolist (mode cleaner-modes)
      (let* ((mode-str (cdr (assq mode mode-line-cleaner-alist)))
             (old-mode-str (cdr (assq mode minor-mode-alist))))
	;; Minor modes
	(when old-mode-str
	  (setcar old-mode-str (concat (when mode-str " ") mode-str)))

	;; Major mode
	(when (eq mode major-mode)
	  (setq mode-name mode-str))))))

;; Run this after every major mode change
(add-hook 'after-change-major-mode-hook 'mode-line-cleaner)

;; Provide module
(provide 'mode-line-cleaner)
