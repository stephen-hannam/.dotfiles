;; evil
(defun in-all-visible-buffers-search-highlight-word-at-point ()
  (interactive)
  (let* (
         (word (evil-find-word t))
         ($op)
        )
    (save-window-excursion
      (dolist (buffer (usr/visible-buffers-buffers-list))
        (switch-to-buffer buffer)
        (setq $op (point))
        (if (search-forward word nil t)
              (evil-ex-search-word-forward)
	  (when (search-backward word nil t)
	    (evil-ex-search-word-forward)))))
      (goto-char $op) 
    )
)

;; makes certain Vim like operator-state sequences operate on entire word
;; yw -> yiw, dw -> diw
;; this will only apply to the below specified commands; evil-yank/delete/change
;; source : https://stackoverflow.com/questions/37238920/key-mapping-in-evil-mode-emacs
(defun usr/evil-motion-range (orig-fun &rest args)
  (if (not (memq this-command '(evil-yank evil-delete)))
      (apply orig-fun args)
    (let* ((orig-keymap evil-operator-state-local-map)
           (evil-operator-state-local-map (copy-keymap orig-keymap)))
      (define-key evil-operator-state-local-map "w" "iw")
      (apply orig-fun args))))

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-integration t)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-want-fine-undo t)
  ;;(setq evil-undo-system 'undo-tree)
  :config
  ;; much more vim like search interface when ex-mode / is used
  (evil-select-search-module 'evil-search-module 'evil-search)
  (evil-set-initial-state 'message-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)
  (setq evil-emacs-state-cursor '("#81a2be" box))
  (setq evil-normal-state-cursor '("#81a2be" box))
  (setq evil-visual-state-cursor '("orange" box))
  (setq evil-insert-state-cursor '("green" bar))
  (setq evil-replace-state-cursor '("red" box))
  (setq evil-operator-state-cursor '("red" hollow))
  ;; evil key-bindings I DON'T want
  (define-key evil-motion-state-map ";" nil) ;; reuse for commenting

  (evil-define-key '(normal visual) 'evil-motion-state-map (kbd "*") 'in-all-visible-buffers-search-highlight-word-at-point)

  (evil-define-key '(normal) 'global  (kbd "M-.") #'helpful-at-point)
  (evil-define-key '(normal visual) 'global (kbd "C-e") 'exit-recursive-edit)
  (evil-define-key '(insert) 'global (kbd "C-g") 'evil-normal-state)
  (evil-define-key '(normal) 'global (kbd "r") 'evil-replace-state)
  (evil-define-key '(normal) 'global (kbd "<left>") 'evil-backward-word-begin)
  (evil-define-key '(normal) 'global (kbd "<right>") 'evil-forward-word-end)
  (evil-define-key '(normal) 'global (kbd "S-<up>") 'evil-backward-paragraph)
  (evil-define-key '(normal) 'global (kbd "S-<down>") 'evil-forward-paragraph)
  (evil-define-key '(normal) 'global (kbd "RET") (lambda() (interactive) (evil-insert-newline-below)))
  (evil-ex-define-cmd "q" 'usr/delete-window-maybe-kill-buffer-maybe-delete-frame)
  (evil-ex-define-cmd "aq" 'usr/kill-other-buffers)
  ;; Need to type out :quit to close emacs
  (evil-ex-define-cmd "quit" 'evil-quit)

  (advice-add 'evil-operator-range :around #'usr/evil-motion-range)
  (evil-mode 1)
)

(use-package evil-anzu
  :after evil
)

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init)
)

(use-package evil-mc
  :defer 1
  :after evil
  :config
  (setq evil-mc-cursor-variables
        (mapcar
         (lambda (s)
           (remove 'register-alist
                   (remove 'evil-markers-alist
                           (remove evil-was-yanked-without-register s))))
         evil-mc-cursor-variables))
  ;; Redefine this function to fix cursor misalignment issues.
  ;; e.g. With multiple cursors, visually select one character and change.
  ;;      With the original `evil-mc-execute-evil-change' the fake cursors would jump one
  ;;      character to the left, incorrectly.
  (defun evil-mc-execute-evil-change ()
    "Execute an `evil-change' comand."
    (let ((point (point)))
      (evil-with-state normal
        (unless (eq point (point-at-bol))
          (evil-forward-char 1 nil t)) ; Perhaps this behavior depends on `evil-move-cursor-back'?
        (evil-mc-execute-with-region-or-macro 'evil-change)
        (evil-maybe-remove-spaces nil))))
  (defun usr/mc-toggle-cursors ()
    (interactive)
    (if (evil-mc-frozen-p)
        (evil-mc-resume-cursors)
      (evil-mc-pause-cursors))
  )

  (evil-define-key '(normal visual) 'global (kbd "R") 'evil-mc-undo-all-cursors)
  (evil-define-key '(normal visual) 'global (kbd "!") 'usr/mc-toggle-cursors)

  (global-evil-mc-mode 1)
)

;; for incr/decr numbers in various patterns; dec, oct, hex, bin
(use-package evil-numbers
  :defer t
  :after evil
  :config
  (evil-define-key '(normal visual) 'global (kbd "+") 'evil-numbers/inc-at-pt-incremental)
  (evil-define-key '(normal visual) 'global (kbd "-") 'evil-numbers/dec-at-pt-incremental)
  (evil-define-key '(normal visual) 'global (kbd "C-+") 'evil-numbers/inc-at-pt)
  (evil-define-key '(normal visual) 'global (kbd "C--") 'evil-numbers/dec-at-pt)
)

(use-package evil-nerd-commenter
  :after evil
  :config
  (evil-define-key '(normal visual) 'global ";" 'evilnc-comment-or-uncomment-lines)
  (evil-define-key '(normal visual) 'global (kbd "C-;") 'evilnc-comment-or-uncomment-paragraphs)
)

(provide 'evil-init)
