;; themes, mode-line, solaire, etc

(use-package git-gutter
  :config
  (global-git-gutter-mode 1)
)

;; Add some visual flair to the modeline enhancements
(use-package spaceline
  :config
  (require 'spaceline-config)
  (spaceline-toggle-minor-modes-off)
  (spaceline-toggle-global-off)
  (spaceline-toggle-buffer-encoding-off)
  (spaceline-toggle-buffer-encoding-abbrev-off)
  (setq powerline-default-separator 'slant)
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  (spaceline-define-segment line-column
    "The current line and column numbers."
    "l:%l c:%c")
  (spaceline-define-segment time
    "The current time."
    (format-time-string "%H:%M"))
  (spaceline-define-segment date
    "The current date."
    (format-time-string "%h %d"))
  (spaceline-emacs-theme 'date 'time)
)

(use-package spaceline-all-the-icons
  :after '(spaceline all-the-icons)
  :config (spaceline-all-the-icons-theme)
)

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

(use-package hl-todo
  :defer t
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(("TODO"       warning bold)
          ("FIXME"      error bold)
          ("HACK"       font-lock-constant-face bold)
          ("REVIEW"     font-lock-keyword-face bold)
          ("NOTE"       success bold)
          ("DEPRECATED" font-lock-doc-face bold)))
)

;;(use-package vscode-icon
;;  :commands (vscode-icon-for-file)
;;)

(provide 'skin-init)
