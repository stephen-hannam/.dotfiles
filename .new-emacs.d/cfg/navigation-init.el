;; avy, 'dired-stuff', ggtags, dashboard, ?ace-window?, chords, key-chords
;; direx + popwin for sidebar action ? or direx-k

;; TODO: look into winner-mode to manipulate windows configurations on a stack or in a linked-list ... etc

(use-package avy
  :bind ("M-s" . avy-goto-char)
)

;; TODO: assess: (define-key isearch-mode-map (kbd "C-'") 'avy-isearch) <<- integrate avy with isearch

(use-package ace-window
  :init (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
              aw-char-position 'left
              aw-ignore-current nil
              aw-leading-char-style 'char
              aw-scope 'frame)
  :bind (
         ("M-O" . ace-swap-window)
        )
)

(use-package expand-region
  :bind ("C-=" . er/expand-region)
)

;;(use-package ibuffer-sidebar
;;  :ensure nil
;;  :commands (ibuffer-sidebar-toggle-sidebar)
;;  :config
;;  (setq ibuffer-sidebar-use-custom-font t)
;;  (setq ibuffer-sidebar-face `(:family "Helvetica" :height 140))
;;)

(provide 'navigation-init)
