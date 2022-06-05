;; inspired by: https://hungyi.net/posts/hydra-for-evil-mc/

(defun usr/mc-toggle-cursors ()
  (interactive)
  (if (evil-mc-frozen-p)
      (evil-mc-resume-cursors)
    (evil-mc-pause-cursors))
)

(defun usr/mc-select-matches ()
  (interactive)
  (evil-mc-execute-for-all-cursors
   (lambda (args)
     (interactive)
     (when (thing-at-point-looking-at (caar evil-mc-pattern))
       (if (alist-get :real args)
           (progn
             (goto-char (match-beginning 0))
             (evil-visual-char)
             (goto-char (- (match-end 0) 1)))
         (setq region (evil-mc-create-region
                       (match-beginning 0)
                       (match-end 0)
                       'char))))))
)

(defun usr/mc-toggle-cursor-at-pos ()
  (interactive)
  (unless (evil-mc-undo-cursor-at-pos (point))
    (evil-mc-make-cursor-here))
)

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

(evil-define-key '(normal visual) 'global (kbd "R") 'evil-mc-undo-all-cursors)
(evil-define-key '(normal visual) 'global (kbd "!") 'usr/mc-toggle-cursors)
(global-evil-mc-mode 1)

(provide 'mc-hydra)
