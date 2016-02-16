;; Frame geometry
(setq default-frame-alist '((width . 80) (height . 25)))

;; Font
;; TODO - Create fallbacks
(set-face-attribute 'default nil :font "Input-14")

;; Theme
(setq monokai-use-variable-pitch nil)

(load-theme 'monokai t)

;; Provide module
(provide 'appearance)
