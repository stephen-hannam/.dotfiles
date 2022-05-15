(defun usr/add-to-list-multiple (list to-add)
  "Add multiple items to LIST.
Allows for adding a sequence of items to the same list, rather
than having to call `add-to-list' multiple times."
  (interactive)
  (dolist (item to-add)
    (add-to-list list item)))

(provide 'user-misc-cmds)
