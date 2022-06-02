;; avy, 'dired-stuff', ggtags, dashboard, ?ace-window?, chords, key-chords
;; direx + popwin for sidebar action ? or direx-k

;; TODO: get some key-chords defined

(use-package use-package-chords
  :disabled
  :config 
  (key-chord-mode 1)
)

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
  :bind (("M-o" . ace-window)
         ("M-O" . ace-swap-window))
)

(provide 'navigation-init)
