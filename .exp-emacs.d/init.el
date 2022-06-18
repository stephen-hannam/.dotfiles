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

    (load-theme 'tango-dark)

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

    (setq gc-cons-threshold 800000)
)
