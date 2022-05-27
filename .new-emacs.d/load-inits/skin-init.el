;; themes, mode-line, solaire, etc

;; Add some visual flair to the modeline enhancements
(use-package spaceline
  :config
  (use-package spaceline-config
    :config
    (spaceline-toggle-minor-modes-off)
    (spaceline-toggle-buffer-encoding-off)
    (spaceline-toggle-buffer-encoding-abbrev-off)
    (setq powerline-default-separator 'rounded)
    (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
    (spaceline-define-segment line-column
      "The current line and column numbers."
      "l:%l c:%2c")
    (spaceline-define-segment time
      "The current time."
      (format-time-string "%H:%M"))
    (spaceline-define-segment date
      "The current date."
      (format-time-string "%h %d"))
    (spaceline-toggle-time-on)
    (spaceline-emacs-theme 'date 'time)
    )

(use-package spaceline-all-the-icons
  :after spaceline
  :config (spaceline-all-the-icons-theme)
  (spaceline-all-the-icons--setup-neotree)
  )

(use-package solaire-mode
  :hook
  (after-make-frame-functions .  (lambda () (load-theme 'doom-tomorrow-night t)
                                   (solaire-mode-swap-faces-maybe)))
  (minibuffer-setup . solaire-mode-fix-minibuffer)
  :custom  
  (solaire-global-mode +1)
  )

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  )

(use-package rainbow-mode
  :defer 1
  :delight
  :hook ((prog-mode text-mode) . rainbow-mode)
  )

(use-package all-the-icons
  :defer t)

(use-package all-the-icons-dired
  :after all-the-icons
  :hook
  ((dired-mode . all-the-icons-dired-mode)
   (dired-single-mode . all-the-icons-dired-mode)
   (wdired-change-to-wdired-mode . all-the-icons-dired-mode)
   )
  )

(provide 'skin-init)
