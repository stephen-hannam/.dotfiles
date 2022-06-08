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

(provide 'dired-init)
