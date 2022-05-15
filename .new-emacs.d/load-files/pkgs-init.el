;; config (not keybindings, but hooks, etc) packages installed (by straight.el) in init.el
;; in here, also activate global modes that need to be activate at start up
;; TODO: answer Q -> do bindings (et al) need to be set before or after starting the mode?

;; consult: editing, register, navigation, search, grep/find, histories, modes, etc
;; https://github.com/minad/consult
;; -- consult-line: alt to swiper
;; -- consult-buffer: nice buffer switching

;; evil
(with-eval-after-load 'evil
  (setq evil-emacs-state-cursor '("#81a2be" box))
  (setq evil-normal-state-cursor '("#81a2be" box))
  (setq evil-visual-state-cursor '("orange" box))
  (setq evil-insert-state-cursor '("green" bar))
  (setq evil-replace-state-cursor '("red" box))
  (setq evil-operator-state-cursor '("red" hollow))
  )

(evil-mode t)
(provide 'pkgs-init)
