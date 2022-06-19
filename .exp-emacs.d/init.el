(let ((file-name-handler-alist nil))
    (require 'subr-x)
    (defconst emacs-private-dir
      (if-let (emacsdir (getenv-internal "EMACSDIR"))
          (expand-file-name (file-name-as-directory emacsdir))
        (or (let ((xdgdir
                   (expand-file-name "emacs/"
                                     (or (getenv-internal "XDG_CONFIG_HOME")
                                         "~/.config"))))
              (if (file-directory-p xdgdir) xdgdir))
            "~/.emacs.d/")))

    (add-to-list 'load-path (concat emacs-private-dir "modules"))

    (load-theme 'tango-dark)

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
    
    (column-number-mode)
    (global-display-line-numbers-mode t)
    (delete-selection-mode t)
    (show-paren-mode t)
    (global-prettify-symbols-mode 1) ;; shows "lambda" as "λ"
    (defalias 'yes-or-no-p 'y-or-n-p)

    (setq show-paren-when-point-inside-paren t
          show-paren-delay 0
          show-paren-style 'mixed
          windmove-wrap-around t)

    (set-face-attribute 'default nil :height 100)

    ;; Initialize package sources
    (require 'package)
    
    (setq package-archives '(("melpa". "https://melpa.org/packages/")
                             ("org"  . "https://orgmode.org/elpa/")
                             ("elpa" . "https://elpa.gnu.org/packages/")))
    
    (package-initialize)
    (unless package-archive-contents
      (package-refresh-contents))
    
    ;; Initialize use-package on non-Linux platforms
    (unless (package-installed-p 'use-package)
      (package-install 'use-package))
    
    (require 'use-package)
    (setq use-package-always-ensure t)
    (setq use-package-verbose t)

    (require 'expand-surround)
    (require 'aides)

    (setq gc-cons-threshold 800000)
)
