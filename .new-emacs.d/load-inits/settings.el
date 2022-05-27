;; settings.el
(setq read-process-output-max (* 1024 1024)
      lexical-binding t)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

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

(set-charset-priority 'unicode)
(setq locale-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))
(set-language-environment "UTF-8")

(setq display-time-24hr-format t)
(display-time-mode 1)

(menu-bar-mode -1) ; Disable menu bar
(scroll-bar-mode -1) ; Disable visual scrollbar
(tool-bar-mode -1) ; Disable toolbar
(tooltip-mode -1) ; Disable tooltips
(set-fringe-mode 10) ; Give some breathing room

(load-theme 'tango-dark t)
(set-face-attribute 'default nil :height 100)

(column-number-mode)
(global-display-line-numbers-mode t)
(delete-selection-mode t)
(show-paren-mode t)

(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)
;; makes help buffer use the same window every time
(setq display-buffer-alist
      `((,(rx bos (or "*Apropos*" "*Help*" "*helpful" "*info*" "*Summary*") (0+ not-newline))
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

;; You will most likely need to adjust this font size for your system!
(defvar usr/default-font-size 90)
(defvar usr/default-variable-font-size 110)

;;(ignore-errors (set-frame-font "Menlo-12")) ;; mono-spaced font ... ?
(set-face-attribute 'default nil :font "Fira Code Retina" :height usr/default-font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height usr/default-font-size)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height usr/default-variable-font-size :weight 'regular)

(provide 'settings)
