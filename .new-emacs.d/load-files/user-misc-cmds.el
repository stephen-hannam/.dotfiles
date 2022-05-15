(defun usr/add-to-list-multiple (list to-add)
  "Add multiple items to LIST.
Allows for adding a sequence of items to the same list, rather
than having to call `add-to-list' multiple times."
  (interactive)
  (dolist (item to-add)
    (add-to-list list item)))

(defun usr/evil-motion-range (orig-fun &rest args)
    "makes certain Vim like operator-state sequences operate on entire word
    yw -> yiw, dw -> diw
    this will only apply to the below specified commands; evil-yank/delete/change
    source : https://stackoverflow.com/questions/37238920/key-mapping-in-evil-mode-emacs"
  (if (not (memq this-command '(evil-yank evil-delete)))
      (apply orig-fun args)
    (let* ((orig-keymap evil-operator-state-local-map)
           (evil-operator-state-local-map (copy-keymap orig-keymap)))
      (define-key evil-operator-state-local-map "w" "iw")
      (apply orig-fun args))))

(provide 'user-misc-cmds)
