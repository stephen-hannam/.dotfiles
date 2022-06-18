(use-package expand-region
  :bind ("C-=" . er/expand-region)
)

(use-package wrap-region
  :hook
  (after-init . wrap-region-global-mode)
)

(provide 'expand-surround)
