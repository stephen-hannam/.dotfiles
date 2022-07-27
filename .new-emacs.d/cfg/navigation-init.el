;; avy, 'dired-stuff', dashboard, chords, key-chords
;; direx + popwin for sidebar action ? or direx-k

;; TODO: look into winner-mode to manipulate windows configurations on a stack or in a linked-list ... etc

;; TODO: bind this in evil visual+normal modes only
(use-package expand-region
  :bind ("C-=" . er/expand-region)
)

;; TODO: bind this in evil visual+normal modes only
(use-package avy
  :bind ("M-s" . avy-goto-char)
)

;;(use-package ibuffer-sidebar
;;  :ensure nil
;;  :commands (ibuffer-sidebar-toggle-sidebar)
;;  :config
;;  (setq ibuffer-sidebar-use-custom-font t)
;;  (setq ibuffer-sidebar-face `(:family "Helvetica" :height 140))
;;)

(provide 'navigation-init)
