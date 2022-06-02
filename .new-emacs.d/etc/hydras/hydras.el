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

;; ↓ from https://hungyi.net/posts/hydra-for-evil-mc/
(defhydra hydra-evil-mc (
                         :color pink
                         :hint nil
                         :pre (evil-mc-pause-cursors)
                         :post (progn
                                 (evil-mc-resume-cursors)
                                 (when evil-mc-pattern (srh/mc-select-matches))))
  "
_m_: make & next     _n_: move to next     _a_:  match all     _!_: freeze / unfreeze     _q_: quit
_M_: make & prev     _N_: move to prev     _R_: remove all     _f_: make / remove
Current pattern: %s(replace-regexp-in-string \"%\" \"%%\" (or (caar evil-mc-pattern) \"\"))  
"
  ("a" #'evil-mc-make-all-cursors)
  ("m" #'evil-mc-make-and-goto-next-match)
  ("M" #'evil-mc-make-and-goto-prev-match)
  ("n" (lambda() (interactive) (evil-mc-goto-cursor (evil-mc-find-next-cursor) t)))
  ("N" (lambda() (interactive) (evil-mc-goto-cursor (evil-mc-find-prev-cursor) t)))
  ("f" (lambda() (interactive) (srh/toggle-cursor-at-pos)))
  ("R" #'evil-mc-undo-all-cursors)
  ("!" (lambda() (interactive) (srh/stand-in-function tgl-all-mc-msg)))
  ("q" (message "") :color blue)
  ("<escape>" (message "") :color blue)
  )

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

(defhydra hydra-dired-options-4-dummies (
                                         :color pink
                                         :hint nil
                                         :pre (lambda() (interactive) (force-mode-line-update 'ALL))
                                         :post (lambda() (interactive) (force-mode-line-update nil))
                                         )
  "
^^^^applies to Marked xor thing-@-Point         ^to thing-@-Point ^^^                                ^to Buffer or All-in-Buffer^
^^^^------------------------------------        ^-----------------------------------------^^^        ^----------------------------^
_!_: shell cmd           _&_: async cmd         _m_: mark               _k_: hide line               _<kp-0>_: ↑ to parent dir
_D_: Delete              _G_: CHGRP             _u_: unmark             _w_: copy then hide          _<f5>_: [or g-r] revert buffer
_R_: Move                _O_: CHOWN             _c_: compress           _=_: diff                    _t_: invert marks
_K_: Hide Lines          _M_: CHMOD             _+_: MKDIR              _e_: new file                _h_: show/hide dotfiles
_F_: Open All            _Z_: TAR               _o_: open to other window [or S-RET] ^^              _(_: [or ,/] hide details
_L_: Load into Emacs     _T_: TOUCH                           
_C_: Copy                _H_: LN (hard)
^^                       _S_: LN (soft)          ^^ ^^                                             | _%_ regex : copy, rename, etc
_x_: delete flagged      _U_: Unmark All        _<tab>_: subtree expand/collapse ^^          OTHER | _*_ marks : all mark commands
                                                                                  ^^^^^^^^PREFIXES | _:_ crypt : encrypt/decrypt
Current switches: %s(format dired-listing-switches)
  "
  ("<kp-0>" (lambda() (interactive) (dired-single-buffer "..")))
  ("C" #'dired-do-copy)
  ("D" #'dired-do-delete)
  ("G" #'dired-do-chgrp)
  ("H" #'dired-do-hardlink)
  ("L" #'dired-do-load)
  ("M" #'dired-do-chmod)
  ("O" #'dired-do-chown)
  ("P" #'dired-do-print)
  ("R" #'dired-do-rename)
  ("S" #'dired-do-symlink)
  ("T" #'dired-do-touch)
  ("Z" #'dired-do-compress)
  ("c" #'dired-do-compress-to)
  ("!" #'dired-do-shell-command)
  ("&" #'dired-do-async-shell-command)
  ("m" #'dired-mark)
  ("h" #'dired-hide-dotfiles-mode)
  ("t" #'dired-toggle-marks)
  ("(" #'dired-hide-details-mode)
  ("o" #'dired-find-file-other-window)
  ("w" #'dired-copy-filename-as-kill)
  ("x" #'dired-do-flagged-delete)
  ("+" #'dired-create-directory)
  ("=" #'dired-diff)
  ("e" #'dired-create-empty-file)
  ("t" #'dired-toggle-marks)
  ("k" (lambda() (interactive) (dired-do-kill-lines 1)))
  ("K" #'dired-do-kill-lines)
  ("u" #'dired-unmark)
  ("U" #'dired-unmark-all-marks)
  ("F" (lambda() (interactive) (dired-do-find-marked-files)) :color blue)
  ("<tab>" #'dired-subtree-toggle)
  ("<f5>" #'revert-buffer)
  ("%" (call-interactively (srh/simulate-key-press "%")) :color blue)
  ("*" (call-interactively (srh/simulate-key-press "*")) :color blue)
  (":" (call-interactively (srh/simulate-key-press ":")) :color blue)
  ("q" (message "") :exit t)
  ("<escape>" (message "") :exit t)
  )

(defhydra hydra-tabularize-shortcuts (
                                      :exit t
                                      :hint nil)
  "
  _,_ before comma   _._ period          _>_ => 
  _;_ after comma    _'_ single-quote   
  "
  (","  (align-to-comma-before))
  (";"  (align-to-comma-after))
  ("."  (align-to-period))
  ("'"  (align-to-single-quote))
  (">"  (align-to-hash))
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
