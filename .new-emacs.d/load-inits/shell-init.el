;; vterm, `zsh plugins',
;; most of this initially coming from: https://config.daviwil.com/emacs
;; also look in to: http://rawsyntax.com/blog/learn-emacs-zsh-and-multi-term/

(use-package sudo-edit
  :defer t)

(use-package sh-script
  :ensure nil
  :hook (after-save . executable-make-buffer-file-executable-if-script-p)
  )

(use-package vterm
  :commands vterm
  :custom
  (vterm-max-scrollback 10000)
  )

(use-package esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode)
  :custom
  (esh-autosuggest-delay 0.5)
  )

(use-package eshell-syntax-highlighting
  :after esh-mode
  :config
  (eshell-syntax-highlighting-global-mode +1)
  )


(provide 'shell-init)
