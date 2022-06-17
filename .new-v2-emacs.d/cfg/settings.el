;; settings.el
(setq read-process-output-max (* 1024 1024)
      lexical-binding t)

(custom-set-variables
 '(default-frame-alist '((fullscreen . maximized)))) ;; start maximized

(setq inhibit-startup-message t
      initial-scratch-message nil
      sentence-end-double-space nil
      ring-bell-function 'ignore
      use-dialog-box nil
      case-fold-search nil
      compilation-scroll-output t
      load-prefer-newer t
      help-window-select t
      make-backup-files nil
      auto-save-default nil
      create-lockfiles nil
      custom-file null-device)

(setq display-time-24hr-format t)
(display-time-mode 1)

(menu-bar-mode -1) ; Disable menu bar
(scroll-bar-mode -1) ; Disable visual scrollbar
(tool-bar-mode -1) ; Disable toolbar
(tooltip-mode -1) ; Disable tooltips
(set-fringe-mode 10) ; Give some breathing room

(set-face-attribute 'default nil :height 100)

(column-number-mode)
(global-display-line-numbers-mode t)
(delete-selection-mode t)
(show-paren-mode t)

;; makes help buffer use the same window every time
(setq display-buffer-alist
      `((,(rx bos (or "*Apropos*" "*Help*" "*helpful" "*info*" "*Info*" "*Summary*") (0+ not-newline))
         (display-buffer-reuse-mode-window display-buffer-pop-up-window)
         (mode apropos-mode help-mode helpful-mode Info-mode Man-mode))))

(global-prettify-symbols-mode 1) ;; shows "lambda" as "λ"
(defalias 'yes-or-no-p 'y-or-n-p)

(setq show-paren-when-point-inside-paren t
      show-paren-delay 0
      show-paren-style 'mixed
      windmove-wrap-around t)

;; FIXME: save-places may not be working in daemon mode
(save-place-mode 1)
(setq save-place-file (concat emacs-private-dir ".places"))
    
(usr/add-to-list-multiple 'auto-mode-alist '(
                                             ("\\.*rc$" . conf-unix-mode)
                                             ("\\.bash*" . conf-unix-mode)
                                             ("/etc/**/bash*" . conf-unix-mode)
                                             ("~/\\.*" . conf-unix-mode)
                                             ))

(setq ilog-print-lambdas t)

(provide 'settings)
