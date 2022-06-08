(use-package general
  :defer t
  :config
  (general-evil-setup)
  ;;(general-create-definer holy/leader-keys
  ;;  :states '(normal insert visual emacs)
  ;;  :keymaps 'override
  ;;  :prefix "SPC"
  ;;  :global-prefix "C-SPC")
  ;;(holy/leader-keys
  ;;  "/"  '(swiper :wk "Swiper")
  ;;  "d"  '(:ignore t :wk "Emacs Debug-Logging")
  ;;  "dd" '(srh/open-command-log :wk "open command log")
  ;;  "dD" '(clm/close-command-log-buffer :wk "hide command log")
  ;;  "g"  '(:ignore t :wk "Magit")
  ;;  "gg" '(magit-status :wk "magit-status")
  ;;)
  (general-create-definer evil/leader-keys
    :states '(normal visual)
    :keymaps 'override
    :prefix ","
    :global-prefix "C-,")
  (evil/leader-keys
    "s"   '(swap-regions :wk "Swap-regions")
    "c"   '(cycle-capitalizations :wk "Cycle-capitalizations")
    ;; don't display wk, my muscle memory is forever
    "SPC" '(usr/in-all-visible-buffers-search-unhighlight :wk t) 
    ":"   '(align-to-colon  :wk t)
    "<"   '(align-to-non-blocking-assign :wk t)
    ">"   '(align-to-hash :wk t)
    "="   '(align-to-equals  :wk t)
    "("   '(align-to-open-paren  :wk t)
    "["   '(align-to-open-bracket  :wk t)
    "."   '(align-to-period  :wk t)
    ","   '(hydra-align-comma-before-or-after/body  :wk t)
    ;; use wk for these
    "S"   '(hydra-text-scale/body :wk "Scale Text →")
    "m"   '(hydra-evil-mc/body :wk "Multiple Cursors →")
    "e"   '(hydra-extended-chars/body :wk "Extended Characters →")
    "f"   '(hydra-folder-shortcuts/body :wk "Folder Shortcuts →")
  )
  (general-create-definer dired-4-dummies/leader-keys
    :states '(normal visual)
    :keymaps 'dired-mode-map
    :prefix "?"
    :global-prefix "C-?")
  (dired-4-dummies/leader-keys
    "" '(hydra-dired-options-4-dummies/body :wk t)
  )
)

(provide 'general-init)
