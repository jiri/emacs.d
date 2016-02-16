;; Eshell
(setq eshell-path-env (concat "/usr/local/bin:" eshell-path-env))

;; Better `pwd'
(defun fish-path (path max-len)
  "Return a potentially trimmed-down version of the directory PATH, replacing
parent directories with their initial characters to try to get the character
length of PATH (sans directory slashes) down to MAX-LEN."
  (let* ((components (split-string (abbreviate-file-name path) "/"))
         (len (+ (1- (length components))
                 (cl-reduce '+ components :key 'length)))
         (str ""))
    (while (and (> len max-len)
                (cdr components))
      (setq str (concat str
                        (cond ((= 0 (length (car components))) "/")
                              ((= 1 (length (car components)))
                               (concat (car components) "/"))
                              (t
                               (if (string= "."
                                            (string (elt (car components) 0)))
                                   (concat (substring (car components) 0 2)
                                           "/")
                                 (string (elt (car components) 0) ?/)))))
            len (- len (1- (length (car components))))
            components (cdr components)))
    (concat str (cl-reduce (lambda (a b) (concat a "/" b)) components))))

;; Make `clear-scrollback' behave more like `ansi-term'
(defun sindriava/clear-scrollback ()
  (interactive)
  (eshell/clear-scrollback)
  (eshell-send-input nil nil t))

(define-key eshell-mode-map (kbd "C-c C-l") 'sindriava/clear-scrollback)

;; A better prompt
(defun sindriava/eshell-prompt ()
  "Custom Eshell prompt function."
  (concat (propertize "λ " 'face '(:foreground "#F92672"))
          (fish-path (eshell/pwd) 1)
          " "))

;; Make `eshell-prompt' use default font color
(set-face-attribute 'eshell-prompt nil
                    :foreground 'unspecified
                    :weight 'unspecified)

;; Set the prompt
(setq eshell-prompt-regexp "^λ [^ ]* ")
(setq eshell-prompt-function 'sindriava/eshell-prompt)

;; Provide module
(provide 'eshell-config)
