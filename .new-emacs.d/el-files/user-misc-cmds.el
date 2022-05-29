(defun usr/add-to-list-multiple (list to-add)
  "Add multiple items to LIST.
Allows for adding a sequence of items to the same list, rather
than having to call `add-to-list' multiple times."
  (interactive)
  (dolist (item to-add)
    (add-to-list list item))
)

(defun usr/display-interaction-log ()
  (interactive)
  (progn
    (interaction-log-mode 'toggle)
    (display-buffer-in-side-window (get-buffer "*Emacs Log*")
                                   '((side . right))))
)

(defun usr/delete-window-maybe-kill-buffer-maybe-delete-frame ()
  (interactive)
  (if (eq (count-windows) 1)
      (evil-ex-call-command nil "quit" nil)
    (delete-window-maybe-kill-buffer))
)

(defun map-all-evil-states (keys action)
  "maps key combination to action for all evil modes"
  (define-key evil-normal-state-map keys action)
  (define-key evil-insert-state-map keys action)
  (define-key evil-visual-state-map keys action)
)

(defun map-n-v-evil-states (keys action)
  (define-key evil-normal-state-map keys action)
  (define-key evil-visual-state-map keys action)
)

(defun map-n-i-evil-states (keys action)
  (define-key evil-normal-state-map keys action)
  (define-key evil-insert-state-map keys action)
)

(defun usr/stand-in-function (&optional msg ret)
  (interactive)
  (when msg (message "%s" msg))
  (let ((fret))
    (if ret
        (setq fret ret)
      (setq fret t))
    fret)
)

(provide 'user-misc-cmds)
