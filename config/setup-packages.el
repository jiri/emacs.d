;; Add MELPA to `package-archives'
(unless (assoc "melpa" package-archives)
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/") t))

;; Automatically install packages
(when (not package-archive-contents)
  (package-refresh-contents))

(defun installed-from-archive-p (pkg)
  "Check if package was installed from an external archive."
  (assq pkg package-alist))

(defun install-packages (pkgs)
  "Install packages in `pkgs'."
  (dolist (p pkgs)
    (when (not (installed-from-archive-p p))
      (package-install (cadr (assq p package-archive-contents))))))

;; Provide module
(provide 'setup-packages)
