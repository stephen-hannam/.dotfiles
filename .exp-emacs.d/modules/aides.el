
(use-package vertico
  :custom
  (vertico-cycle t)
  :custom-face
  (vertico-current ((t (:background "#3a3f5a"))))
  :config
  (vertico-mode)
)

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

(use-package marginalia
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :config
  (marginalia-mode)
  :bind
  (("<tab>" . minibuffer-complete))
)

(use-package orderless
  :custom
  (completion-styles '(substring partial-completion orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles . (partial-completion)))))
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
(provide 'aides)
