;; config (not keybindings, but hooks, etc) packages installed (by straight.el) in init.el
;; in here, also activate global modes that need to be activate at start up
;; TODO: answer Q -> do bindings (et al) need to be set before or after starting the mode?

;; consult: editing, register, navigation, search, grep/find, histories, modes, etc
;; https://github.com/minad/consult
;; -- consult-line: alt to swiper
;; -- consult-buffer: nice buffer switching

;; evil
(use-package evil
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-integration t)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-want-fine-undo t)
  (setq evil-undo-system 'undo-tree)
  :config
  (evil-mode 1)
  ;; much more vim like search interface when ex-mode / is used
  (evil-select-search-module 'evil-search-module 'evil-search)
  (evil-set-initial-state 'message-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)
  )

(use-package evil-anzu
  :after evil
  )

(use-package evil-collection
  :after (evil general)
  :config
  (evil-collection-init)
  (unbind-key "C-." 'evil-normal-state-map)
  )

(with-eval-after-load 'evil
  (setq evil-want-keybinding nil) ;; what does this do?
  (setq evil-want-integration t)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-want-fine-undo t)
  (setq evil-undo-system 'undo-tree)
  (setq evil-emacs-state-cursor '("#81a2be" box))
  (setq evil-normal-state-cursor '("#81a2be" box))
  (setq evil-visual-state-cursor '("orange" box))
  (setq evil-insert-state-cursor '("green" bar))
  (setq evil-replace-state-cursor '("red" box))
  (setq evil-operator-state-cursor '("red" hollow))
  (evil-select-search-module 'evil-search-module 'evil-search)
  (evil-set-initial-state 'message-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)
  )

(with-eval-after-load 'evil-mc
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
  )

(provide 'pkgs-init)
