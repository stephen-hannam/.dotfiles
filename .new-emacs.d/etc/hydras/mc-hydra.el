;; inspired by: https://hungyi.net/posts/hydra-for-evil-mc/
(defvar tgl-all-mc-msg "Does nothing until you leave the mini-buffer")

(defhydra hydra-evil-mc (
                         :color pink
                         :hint nil
                         :pre (evil-mc-pause-cursors)
                         :post (progn
                                 (evil-mc-resume-cursors)
                                 (when evil-mc-pattern (usr/mc-select-matches))))
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
  ("f" (lambda() (interactive) (usr/mc-toggle-cursor-at-pos)))
  ("R" #'evil-mc-undo-all-cursors)
  ("!" (lambda() (interactive) (usr/stand-in-function tgl-all-mc-msg)))
  ("q" (message "") :color blue)
  ("<escape>" (message "") :color blue)
)

(provide 'mc-hydra)
