;; Region settings
(use-package expand-region
  :init
  (setq expand-region-fast-keys-enabled nil)
  :config
  (with-eval-after-load 'hydra
    (defhydra hydra-mark
      (:body-pre (call-interactively 'set-mark-command))

      ;; Paired symbols
      ("P" er/mark-inside-pairs)
      ("p" er/mark-outside-pairs)
      ("Q" er/mark-inside-quotes)
      ("q" er/mark-outside-quotes)

      ;; Syntactic constructs
      ("l" (lambda ()
             "Select current like"
             (interactive)
             (if (and (region-active-p)
                      (not (equal (point) (mark))))
                 (progn
                   (move-end-of-line nil)
                   (forward-char))
               (progn
                 (move-beginning-of-line nil)
                 (set-mark-command nil)
                 (move-end-of-line nil)
                 (forward-char)
                 (setq deactivate-mark nil)))))

      ;; Semantic constructs
      ("d" er/mark-defun)
      ("s" er/mark-symbol)
      ("c" er/mark-comment)

      ;; Operations
      ("k" kill-region :quit t)
      ("w" kill-ring-save :quit t)

      ;; `expand-region'
      ("x" er/expand-region)
      ("c" er/contract-region))

    (global-set-key (kbd "C-SPC") 'hydra-mark/body)))

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

;; Tweak live windows
(defun quit-window-always-kill (f &rest args)
  "Make sure that live windows stay quit"
  (apply f (cons 0 (cdr args))))

(advice-add 'quit-window
            :around 'quit-window-always-kill)

;; Provide module
(provide 'bindings)
