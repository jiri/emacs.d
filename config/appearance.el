;; Frame geometry
(setq default-frame-alist '((width . 80) (height . 25)))

;; Disable GUI cruft
(when (fboundp 'tool-bar-mode) (tool-bar-mode 0))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode 0))

(setq frame-title-format nil)
(setq ring-bell-function 'ignore)

;; Don't wrap lines by default
(setq-default truncate-lines 1)

;; Disable startup screen
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message "sindriava")

;; Make prompts a bit easier to deal with
(defalias 'yes-or-no-p 'y-or-n-p)
(setq confirm-nonexistent-file-or-buffer nil)

;; Center frames on the screen
(defun sindriava/center-frame (&optional frame)
  "Center `frame' on the screen. If it's `nil', center the selected frame."
  (interactive)
  (let ((f (or frame (selected-frame))))
    (set-frame-position f
                        (/ (- (display-pixel-width)
                              (frame-pixel-width f))
                           2)
                        (/ (- (display-pixel-height)
                              (frame-pixel-height f))
                           2))))

;; Center all newly created frames
(push 'sindriava/center-frame after-make-frame-functions)

;; Center the initial frame after Emacs starts
(add-hook 'window-setup-hook (lambda () (sindriava/center-frame (selected-frame))))

;; Font
;; TODO - Create fallbacks
(set-face-attribute 'default nil :font "Input-14")

;; Theme
(setq monokai-use-variable-pitch nil)

(load-theme 'monokai t)

;; Modeline
(require 'mode-line-config)

;; Provide module
(provide 'appearance)
