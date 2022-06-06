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
  :bind (("M-o" . ace-window)
         ("M-O" . ace-swap-window))
)

(use-package dired
  :straight (:type built-in)
  :defer t
  :commands (dired)
  :hook
  (dired-mode . dired-hide-details-mode)
  :bind
  (:map dired-mode-map
   ([f5] . revert-buffer)
   ("/" . dired-hide-details-mode)
   ("h" . dired-hide-dotfiles-mode)
   )
  ([remap dired-mouse-find-file] . mouse-set-point)
  ([remap dired-mouse-find-file-other-frame] . mouse-set-point)
  ([remap dired-mouse-find-file-other-window] . mouse-set-point)
  :custom
  (require 'dired-x) ;; for -mark-extension and few others
  (dired-listing-switches "-AlthvX --group-directories-first")
  (dired-dwim-target 'dired-dwim-target-next)
)

;; cycle view sub-folder and sub-sub-folders in existing buffer
(use-package dired-subtree
  :after dired
  :config
  (bind-key "<tab>" #'dired-subtree-toggle dired-mode-map)
  (bind-key "<backtab>" #'dired-subtree-cycle dired-mode-map)
)

;; fl = font-lock. Provided informative font changes based on file ext
(use-package diredfl
  :after dired
  :config
  (diredfl-global-mode)
)

(use-package dired-hide-dotfiles
  :after dired
  :commands (dired)
  :hook (dired-mode . dired-hide-dotfiles-mode)
)

;; make dired do everything in a single buffer
(use-package dired-single
  :after dired
  :bind
  ([remap dired-find-file] . dired-single-buffer)
  ([remap dired-up-directory] . (lambda() (interactive) (dired-single-buffer "..")))
  ([remap dired-single-buffer-mouse] . mouse-set-point)
)

(with-eval-after-load 'evil-collection
  (evil-collection-define-key 'normal 'dired-mode-map
    (kbd "<kp-0>") 'dired-up-directory)
  (evil-collection-define-key 'normal 'dired-mode-map
    (kbd "o") 'dired-find-file-other-window)
  (evil-collection-define-key 'normal 'dired-mode-map
    (kbd "e") 'dired-create-empty-file)
  (evil-collection-define-key 'normal 'dired-mode-map
    (kbd "G") 'dired-do-chgrp)
  (evil-collection-define-key 'normal 'dired-mode-map
    (kbd "k") '(lambda() (interactive) (dired-do-kill-lines 1)))
  (evil-collection-define-key 'normal 'dired-mode-map
    (kbd "F") '(lambda() (interactive) (dired-do-find-marked-files)))
  (evil-collection-define-key 'normal 'image-dired-thumbnail-mode-map
    (kbd "<right>") 'image-dired-forward-image)
  (evil-collection-define-key 'normal 'image-dired-thumbnail-mode-map
    (kbd "<left>") 'image-dired-backward-image)
  (evil-collection-define-key 'normal 'dired-mode-map
    (kbd "h") 'dired-hide-dotfiles-mode)
  (evil-collection-define-key 'normal 'dired-mode-map
    (kbd "/") 'dired-hide-details-mode)
)

(provide 'navigation-init)
