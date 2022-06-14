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

(use-package all-the-icons)
(use-package s)

(require 'skin-init)
(require 'completions-init)
(require 'navigation-init)
(require 'dired-init)
(require 'help-init)
(require 'general-init)
(require 'evil-init)

(provide 'pkgs-init)
