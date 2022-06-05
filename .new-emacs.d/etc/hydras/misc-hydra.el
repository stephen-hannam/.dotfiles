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

(provide 'misc-hydra)
