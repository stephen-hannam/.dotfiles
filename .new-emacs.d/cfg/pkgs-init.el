(use-package diminish
  :ensure nil
)

(use-package undo-tree
  :bind
  ("<escape>" . undo-tree-visualizer-quit)
  :config
  (setq undo-tree-inhibit-kill-visualizer t)
)

(use-package anzu
  :diminish
  :hook
  (after-init . global-anzu-mode)
)

(use-package s)

(require 'evil-init)
(require 'completions-init)
(require 'navigation-init)
(require 'dired-init)
(require 'help-init)
(require 'skin-init)
(require 'general-init)

(provide 'pkgs-init)
