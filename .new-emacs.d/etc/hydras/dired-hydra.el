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
  ("%" (call-interactively (usr/simulate-key-press "%")) :color blue)
  ("*" (call-interactively (usr/simulate-key-press "*")) :color blue)
  (":" (call-interactively (usr/simulate-key-press ":")) :color blue)
  ("q" (message "") :exit t)
  ("<escape>" (message "") :exit t)
)


(provide 'dired-hydra)
