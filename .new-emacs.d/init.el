(let ((file-name-handler-alist nil))

    (add-hook 'emacs-startup-hook
            (lambda ()
              (message "Emacs ready in %s with %d garbage collections."
                       (format "%.2f seconds"
                               (float-time
                                (time-subtract after-init-time before-init-time)))
                       gcs-done)))

    (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
    (load custom-file)

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
    
    (setq locale-coding-system 'utf-8)
    (setq default-buffer-file-coding-system 'utf-8)
    (set-default-coding-systems 'utf-8)
    (set-terminal-coding-system 'utf-8)
    (set-keyboard-coding-system 'utf-8)
    (set-selection-coding-system 'utf-8)
    (prefer-coding-system 'utf-8)

    (menu-bar-mode -1) ; Disable menu bar
    (scroll-bar-mode -1) ; Disable visual scrollbar
    (tool-bar-mode -1) ; Disable toolbar
    (tooltip-mode -1) ; Disable tooltips
    (set-fringe-mode 10) ; Give some breathing room
    
    (load-theme 'tango-dark t)
    (set-face-attribute 'default nil :height 100)
    
    (global-set-key (kbd "<escape>") 'keyboard-escape-quit)
    
    ;; FIXME: save-places may not be working in daemon mode
    (save-place-mode 1)
    (setq save-place-file (locate-user-emacs-file "places" ".emacs-places"))
    
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
    (add-hook 'emacs-lisp-mode-hook (lambda() (setq mode-name " ξ ")))
    ;; straight.el - bootstrap straight package manager if need be
    
    (defvar bootstrap-version)
    (let ((bootstrap-file
          (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
          (bootstrap-version 5))
      (unless (file-exists-p bootstrap-file)
        (with-current-buffer
            (url-retrieve-synchronously
            "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
            'silent 'inhibit-cookies)
          (goto-char (point-max))
          (eval-print-last-sexp)))
      (load bootstrap-file nil 'nomessage))
    
    (defconst emacs-private-dir
      (if-let (emacsdir (getenv-internal "EMACSDIR"))
          (expand-file-name (file-name-as-directory emacsdir))
        (or (let ((xdgdir
                   (expand-file-name "emacs/"
                                     (or (getenv-internal "XDG_CONFIG_HOME")
                                         "~/.config"))))
              (if (file-directory-p xdgdir) xdgdir))
            "~/.emacs.d/")))
    
    ;; straight.el setup of standard packages from the standard repos; elpa, melpa, etc
    (mapcar 
      #'straight-use-package
      '(
        anzu
        avy
        evil
        evil-mc
        evil-numbers
        evil-nerd-commenter
        evil-anzu
        evil-collection
        evil-surround
        hydra
        dired-single
        dired-subtree
        dired-hide-dotfiles
        all-the-icons
        all-the-icons-dired
        rainbow-mode
        rainbow-delimiters
        beacon
        solaire-mode
        pdf-tools
        which-key
        helpful
        vertico
        orderless
        embark
        consult
        marginalia
        magit
        libgit
        swap-regions
        ;; make executable after save if editing a script file
        sh-script
        ))
    
    (add-to-list 'load-path (concat emacs-private-dir "load-files"))
    (add-to-list 'load-path (concat emacs-private-dir "load-inits"))
    
    (require 'cl)
    (require 'move-border)
    (require 'tabularize)
    (require 'misc-cmds)
    (require 'user-misc-cmds)
    (require 'text-manips)
    (require 'interaction-log)
    
    (require 'pkgs-init)
    ;;(require 'keybindings-init)
    ;;(require 'hydras-init)
    
    (usr/add-to-list-multiple 'auto-mode-alist '(
                                                 ("\\.*rc$" . conf-unix-mode)
                                                 ("\\.bash*" . conf-unix-mode)
                                                 ("/etc/**/bash*" . conf-unix-mode)
                                                 ("~/\\.*" . conf-unix-mode)
                                                 ))
    
    ;; TODO: move the package config code to pkgs-init.el and call it from here

    (setq gc-cons-threshold 800000)
)
