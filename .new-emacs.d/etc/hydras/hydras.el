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


;; TODO: setup machine depedendent hydra-defs
;; ie: for work-stations, folders with fpga-firmware, exanic-software, etc
(defhydra hydra-folder-shortcuts (
                                  :exit t
                                  :hint nil)
  "
^^^^Folders                               ^Files
^^^^-------------------------------       ^----------------^
_w_  Work            _p_  Projects        _t_  Tasks-File
_d_  Documents
_E_  Emacs
_P_  Playground

_._  %s(file-name-directory (or buffer-file-name load-file-name (concat \"file-name-directory for \" (format \"%s\" (current-buffer)) \" not found/unknown\")))
"
  ("w" (evil-window-vsplit nil "/data/shannam/Work"))
  ("E" (evil-window-vsplit nil  stemacs-private-dir))
  ("d" (evil-window-vsplit nil  "~/Documents"))
  ("p" (evil-window-vsplit nil  "~/Projects"))
  ("P" (evil-window-vsplit nil  "~/Playground"))
  ("t" (find-file-other-window srh/tasksfile))
  ("." (evil-window-vsplit nil "."))
  ("q" (message ""))
  ("<escape>" (message ""))
  ("RET" (message "") :color pink)
  )

(defhydra hydra-align-comma-before-or-after (
                                             :exit t
                                             :hint nil)
  "
  "
  ("<left>"  (align-to-comma-before))
  ("<right>"  (align-to-comma-after))
  )

(defhydra hydra-extended-chars (
                                :exit t
                                :hint nil)
  "
_<up>_     ↑      _8_  ★      _a_  α      _f_  ϕ      _k_  κ      _q_  θ      _w_  ω      _D_  Δ      _Q_  Θ
_<down>_   ↓      _6_  ◆      _b_  β      _g_  γ      _l_  λ      _r_  ρ      _x_  ξ      _G_  Γ      _S_  Σ
_<left>_   ←      _I_  ∞      _c_  χ      _h_  η      _m_  μ      _s_  σ      _y_  ψ      _L_  Λ      _W_  Ω
_<right>_  →      ^^          _d_  δ      _i_  ι      _n_  ν      _t_  τ      _z_  ζ      _N_  ∇      _X_  Ξ
^^                ^^          _e_  ε      _j_  φ      _p_  π      _u_  υ      ^^          _P_  Π      _Y_  Ψ

For quick-access use F9 + <key> when non in this hydra-head
"
  ("a" (insert "α"))
  ("b" (insert "β"))
  ("c" (insert "χ"))
  ("d" (insert "δ"))
  ("e" (insert "ε"))
  ("f" (insert "ϕ"))
  ("g" (insert "γ"))
  ("h" (insert "η"))
  ("i" (insert "ι"))
  ("j" (insert "φ"))
  ("k" (insert "κ"))
  ("l" (insert "λ"))
  ("m" (insert "μ"))
  ("n" (insert "ν")) 
  ("p" (insert "π"))
  ("q" (insert "θ"))
  ("r" (insert "ρ"))
  ("s" (insert "σ"))
  ("t" (insert "τ"))
  ("u" (insert "υ"))
  ("w" (insert "ω"))
  ("x" (insert "ξ"))
  ("y" (insert "ψ"))
  ("z" (insert "ζ"))
  ("D" (insert "Δ"))
  ("G" (insert "Γ"))
  ("L" (insert "Λ"))
  ("N" (insert "∇"))
  ("P" (insert "Π"))
  ("Q" (insert "Θ"))
  ("S" (insert "Σ"))
  ("W" (insert "Ω"))
  ("X" (insert "Ξ"))
  ("Y" (insert "Ψ"))
  ("<down>" (insert "↓"))
  ("<left>" (insert "←"))
  ("<right>" (insert "→"))
  ("<up>" (insert "↑"))
  ("8" (insert "★"))
  ("6" (insert "◆"))
  ("I" (insert "∞"))
  )

(defhydra hydra-text-scale (
                            :color pink
                            :hint nil)
  "
text size
_+_ increase    
_-_ decrease
"
  ("+" text-scale-increase)
  ("-" text-scale-decrease)
  ("q" (message "") :exit t)
  ("<escape>" (message "") :exit t)
  )


(provide 'hydras)
