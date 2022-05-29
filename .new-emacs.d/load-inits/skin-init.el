;; themes, mode-line, solaire, etc

;; Add some visual flair to the modeline enhancements
(use-package spaceline
  :config
  (require 'spaceline-config)
  (spaceline-spacemacs-theme)
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

;;(use-package doom-modeline
;;  :hook (after-init . doom-modeline-mode)
;;  :custom
;;  (doom-modeline-height 30)
;;  (doom-modeline-icon (display-graphic-p))
;;  )

(use-package doom-themes
  :defer t
  )

(use-package spacemacs-theme
  :defer t
  :after solaire-mode
  :hook
  (server-after-make-frame . (lambda () (load-theme 'spacemacs-dark t)))
  :init
  (load-theme 'spacemacs-dark t)
  )

;;(use-package spaceline-all-the-icons
;;  :after spaceline
;;  :config (spaceline-all-the-icons-theme)
;;  )

(use-package solaire-mode
  :hook
  ;;(after-make-frame-functions .  (lambda () (load-theme 'doom-tomorrow-night t)
  ;;                                 (solaire-mode-swap-faces-maybe)))
  (after-make-frame-functions .  (lambda () (load-theme 'spacemacs-dark t)
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
  :diminish
  :hook ((prog-mode text-mode) . rainbow-mode)
  )

(use-package all-the-icons
  :defer t
  )

(use-package all-the-icons-dired
  :after all-the-icons
  :hook
  ((dired-mode . all-the-icons-dired-mode)
   (dired-single-mode . all-the-icons-dired-mode)
   (wdired-change-to-wdired-mode . all-the-icons-dired-mode)
   )
  )

(provide 'skin-init)
