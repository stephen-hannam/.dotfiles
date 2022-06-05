;; TODO: convert these general.el mapings into hydras
;;(use-package general
;;  :defer t
;;  :config
;;  (general-evil-setup)
;;  (general-nmap "SPC l" (general-simulate-key "<pause>"))
;;  (general-nmap "SPC f" (general-simulate-key "<XF86WakeUp>"))
;;  (general-create-definer holy/leader-keys
;;    :states '(normal insert visual emacs)
;;    :keymaps 'override
;;    :prefix "SPC"
;;    :global-prefix "C-SPC")
;;  (holy/leader-keys
;;    "/"  '(swiper :wk "Swiper")
;;    "f"  '(:ignore t :wk "Flycheck Commands")
;;    "l"  '(:ignore t :wk "LSP Commands")
;;    "d"  '(:ignore t :wk "Emacs Debug-Logging")
;;    "dd" '(srh/open-command-log :wk "open command log")
;;    "dD" '(clm/close-command-log-buffer :wk "hide command log")
;;    "g"  '(:ignore t :wk "Magit")
;;    "gg" '(magit-status :wk "magit-status")
;;    "o"  '(:ignore t :wk "Org")
;;    "os" '(org-schedule :wk "schedule")
;;    "ot" '(srh/eol-adj-org-time-stamp :wk "timestamp")
;;    "od" '(org-deadline :wk "deadline")
;;    "oo" '(org-agenda :wk "agenda commands")
;;    "o*" '(srh/toggle-org-hide-emph-markers :wk "hide/show emphasis")
;;    )
;;  (general-create-definer evil/leader-keys
;;    :states '(normal insert visual emacs)
;;    :keymaps 'override
;;    :prefix ","
;;    :global-prefix "C-,")
;;  (evil/leader-keys
;;    :predicate '(derived-mode-p 'dired-mode)
;;    "/"   '(srh/toggle-dired-hide-details :wk "Dired-details")
;;    )
;;  (evil/leader-keys
;;    "s"   '(swap-regions :wk "Swap-regions")
;;    "c"   '(cycle-capitalizations :wk "Cycle-capitalizations")
;;    ;; don't display wk, my muscle memory is forever
;;    "SPC" '(srh/evil-nohl :wk t) 
;;    ":"   '(align-to-colon  :wk t)
;;    "<"   '(align-to-non-blocking-assign :wk t)
;;    "="   '(align-to-equals  :wk t)
;;    "("   '(align-to-open-paren  :wk t)
;;    "["   '(align-to-open-bracket  :wk t)
;;    ;; use wk for these
;;    "t"   '(hydra-tabularize-shortcuts/body :wk "Extra Tabularizations →")
;;    "S"   '(hydra-text-scale/body :wk "Scale Text →")
;;    "m"   '(hydra-evil-mc/body :wk "Multiple Cursors →")
;;    "e"   '(hydra-extended-chars/body :wk "Extended Characters →")
;;    "."   '(hydra-folder-shortcuts/body :wk "Folder Shortcuts →")
;;    )
;;  (general-create-definer dired-4-dummies/leader-keys
;;    :states '(normal insert visual emacs)
;;    :keymaps 'dired-mode-map
;;    :prefix "?"
;;    :global-prefix "C-?")
;;  (dired-4-dummies/leader-keys
;;    "" '(hydra-dired-options-4-dummies/body :wk t)
;;    )
;;  )

(require 'mc-hydra)
(require 'dired-hydra)
(require 'misc-hydra)

(provide 'hydras)
