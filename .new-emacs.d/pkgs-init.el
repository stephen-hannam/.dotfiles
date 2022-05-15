;; config (not keybindings, but hooks, etc) packages installed (by straight.el) in init.el
;; in here, also activate global modes that need to be activate at start up
;; TODO: answer Q -> do bindings (et al) need to be set before or after starting the mode?

;; consult: editing, register, navigation, search, grep/find, histories, modes, etc
;; https://github.com/minad/consult
;; -- consult-line: alt to swiper
;; -- consult-buffer: nice buffer switching


(with-eval-after-load 'evil
  (setq evil-emacs-state-cursor '("#81a2be" box))
  (setq evil-normal-state-cursor '("#81a2be" box))
  (setq evil-visual-state-cursor '("orange" box))
  (setq evil-insert-state-cursor '("green" bar))
  (setq evil-replace-state-cursor '("red" box))
  (setq evil-operator-state-cursor '("red" hollow))
  (evil-define-key '(normal) 'global  (kbd "M-.") #'helpful-at-point)
  (evil-define-key '(normal visual insert) 'global  (kbd "M-DEL") 'sp-unwrap-sexp)
  (evil-define-key '(normal visual) 'global (kbd "+") 'evil-numbers/inc-at-pt-incremental)
  (evil-define-key '(normal visual) 'global (kbd "-") 'evil-numbers/dec-at-pt-incremental)
  (evil-define-key '(normal visual) 'global (kbd "C-+") 'evil-numbers/inc-at-pt)
  (evil-define-key '(normal visual) 'global (kbd "C--") 'evil-numbers/dec-at-pt)
  (evil-define-key '(normal visual) 'global (kbd "R") 'evil-mc-undo-all-cursors)
  (evil-define-key '(normal visual) 'global (kbd "!") 'srh/mc-toggle-cursors)
  (evil-define-key '(normal visual) 'global (kbd "C-e") 'exit-recursive-edit)
  (evil-define-key '(insert) 'global (kbd "C-g") 'evil-normal-state)
  (evil-define-key '(normal) 'global (kbd "r") 'evil-replace-state)
  (evil-define-key '(normal) 'global (kbd "<left>") 'evil-backward-word-begin)
  (evil-define-key '(normal) 'global (kbd "<right>") 'evil-forward-word-end)
  (evil-define-key '(normal) 'global (kbd "S-<up>") 'evil-backward-paragraph)
  (evil-define-key '(normal) 'global (kbd "S-<down>") 'evil-forward-paragraph)
  (evil-define-key '(normal visual insert) 'global (kbd "C-<up>")
    (lambda() (interactive) (scroll-other-window-down 1)))
  (evil-define-key '(normal visual insert) 'global (kbd "C-<down>")
    (lambda() (interactive) (scroll-other-window-down -1)))
  (evil-define-key '(normal) 'global (kbd "RET") (lambda() (interactive) (evil-insert-newline-below)))
  ;; :q should kill the current buffer rather than quitting emacs entirely
  (evil-ex-define-cmd "q" 'srh/delete-window-maybe-kill-buffer-maybe-delete-frame)
  (evil-ex-define-cmd "aq" 'kill-other-buffers)
  ;; Need to type out :quit to close emacs
  (evil-ex-define-cmd "quit" 'evil-quit)
  (global-undo-tree-mode)
  (turn-on-undo-tree-mode)
  )


(provide 'pkgs-init)
