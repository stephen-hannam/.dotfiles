;; vterm, `zsh plugins',
;; most of this initially coming from: https://config.daviwil.com/emacs
;; also look in to: http://rawsyntax.com/blog/learn-emacs-zsh-and-multi-term/

(use-package vterm
  :commands vterm
  :config
  (setq vterm-max-scrollback 10000)
  )

(use-package esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode)
  :config
  (setq esh-autosuggest-delay 0.5)
  )

(use-package eshell-syntax-highlighting
  :after esh-mode
  :config
  (eshell-syntax-highlighting-global-mode +1)
  )


(provide 'shell-init)
