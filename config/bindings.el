;; Region settings
(delete-selection-mode)

(use-package expand-region
  :init
  (setq expand-region-fast-keys-enabled nil)
  :config
  (progn
    (global-set-key (kbd "C-SPC") (lambda ()
                                    (interactive)
                                    (if (region-active-p)
                                        (er/expand-region 1)
                                      (set-mark (point)))))
    (global-set-key (kbd "C-S-SPC") 'er/contract-region)))

;; Search
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)

;; Killing & Yanking
(global-set-key (kbd "C-w") 'kill-ring-save)
(global-set-key (kbd "M-w") 'kill-region)

(use-package browse-kill-ring
  :bind ("M-y" . browse-kill-ring))

;; Buffer bindings
(require 'bindings-buffer)

;; Window bindings
(require 'bindings-window)

;; Rebind some defaults
(defun sindriava/beginning-of-line ()
  "Move to first non-whitespace character on `C-a' first."
  (interactive)
  (let ((old-point (point)))
    (back-to-indentation)
    (when (= (point) old-point)
      (beginning-of-line))))

(defun sindriava/kill-buffer ()
  "Kill buffer (except for `*scratch*') and cycle to the next one."
  (interactive)
  (let ((buffer (buffer-name)))
    (if (equal buffer "*scratch*")
        (message "Cannot kill scratch buffer.")
      (progn
        (sindriava/cycle-until-viable 'next-buffer)
        (kill-buffer buffer)))))

(global-set-key (kbd "C-a") 'sindriava/beginning-of-line)
(global-set-key (kbd "C-x k") 'sindriava/kill-buffer)

(global-set-key (kbd "<escape>") 'keyboard-quit)
(global-set-key (kbd "s-q") 'delete-frame)

;; `C-x C-c' is way too easy to press accidentally.
(global-unset-key (kbd "C-x C-c"))
(global-set-key (kbd "C-S-x C-S-c") 'save-buffers-kill-terminal)

;; Tweak live windows
(defun quit-window-always-kill (f &rest args)
  "Make sure that live windows stay quit"
  (apply f (cons 0 (cdr args))))

(advice-add 'quit-window
            :around 'quit-window-always-kill)

;; Provide module
(provide 'bindings)
