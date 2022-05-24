(defun usr/add-to-list-multiple (list to-add)
  "Add multiple items to LIST.
Allows for adding a sequence of items to the same list, rather
than having to call `add-to-list' multiple times."
  (interactive)
  (dolist (item to-add)
    (add-to-list list item)))

(defun usr/display-interaction-log ()
  (interactive)
  (progn
    (interaction-log-mode 'toggle)
    (display-buffer-in-side-window (get-buffer "*Emacs Log*")
                                   '((side . right)))))

(defun usr/delete-window-maybe-kill-buffer-maybe-delete-frame ()
  (interactive)
  (if (eq (count-windows) 1)
      (evil-ex-call-command nil "quit" nil)
    (delete-window-maybe-kill-buffer))
  )

(provide 'user-misc-cmds)
