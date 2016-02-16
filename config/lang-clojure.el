;; Clojure
(require 'clojure-mode-extra-font-locking)

(eval-after-load 'cider-mode '(require 'cider))

;; Enable appropriate minor modes
(add-hook 'clojure-mode-hook 'paredit-mode)

;; Provide module
(provide 'lang-clojure)
