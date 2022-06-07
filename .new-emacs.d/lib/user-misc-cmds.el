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

(defun usr/kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer-and-its-windows (delq (current-buffer) (buffer-list)))
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

(defun usr/visible-buffers-buffers-list ()
  "Given a list of buffers, return buffers which are currently visible."
  (let ((buffers '()))
    (walk-windows (lambda (w) (push (window-buffer w) buffers))) buffers))

(defun usr/in-all-visible-buffers-search-unhighlight ()
  (interactive)
  (let* ((word (evil-find-word t)))
    (save-window-excursion
      (dolist (buffer (usr/visible-buffers-buffers-list))
        (switch-to-buffer buffer)
        (if (search-forward word nil t)
	    (evil-ex-nohighlight)
	  (when (search-backward word nil t)
	      ((evil-ex-nohighlight)))))))
)

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

(provide 'user-misc-cmds)
