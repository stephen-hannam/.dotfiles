;; helpful, info+. which-key, command-log, hl-todo, 'debugging'

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

(use-package command-log-mode
  :defer 1
  :commands command-log-mode
  )

(use-package which-key
  :defer 1
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1.1)
  (setq which-key-separator " → ")
  (setq which-key-prefix-prefix nil)
  (set-face-attribute 'which-key-separator-face nil :weight 'bold :background "black" :foreground "white")
  (set-face-attribute 'which-key-key-face nil :background "black" :foreground "white")
  (set-face-attribute 'which-key-command-description-face nil :foreground "green" :background nil)
  (set-face-attribute 'which-key-group-description-face nil :foreground "green" :background nil)
  (setq which-key-unicode-correction 3)
  (setq which-key-add-column-padding 8)
  (setq which-key-sort-order nil)
  (setq which-key-max-display-columns 4)
  )

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  (
   ([remap describe-function] . helpful-function)
   ([remap describe-command] . helpful-command)
   ([remap describe-variable] . helpful-variable)
   ([remap describe-key] . helpful-key)
   (:map help-mode-map
         ("q" . (lambda() (interactive) (progn
                                     (helpful-kill-buffers)
                                     (delete-window))))
         )
   )
  )

(provide 'help-init)
