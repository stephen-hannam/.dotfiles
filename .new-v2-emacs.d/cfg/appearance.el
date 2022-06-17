;; You will most likely need to adjust this font size for your system!
(defvar usr/default-font-size 90)
(defvar usr/default-variable-font-size 110)

(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;;(ignore-errors (set-frame-font "Menlo-12")) ;; mono-spaced font ... ?
(set-face-attribute 'default nil :font "Fira Code Retina" :height usr/default-font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height usr/default-font-size)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height usr/default-variable-font-size :weight 'regular)

(provide 'appearance)
