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

    (add-to-list 'load-path (concat emacs-private-dir "lib"))
    (add-to-list 'load-path (concat emacs-private-dir "etc"))
    (add-to-list 'load-path (concat emacs-private-dir "etc/themes"))
    (add-to-list 'load-path (concat emacs-private-dir "etc/hydras"))
    (add-to-list 'load-path (concat emacs-private-dir "cfg"))

    (require 'cl)
    (require 'move-border)
    (require 'tabularize)
    (require 'misc-cmds)
    (require 'user-misc-cmds)
    (require 'text-manips)
    (require 'interaction-log)
    (require 'swap-regions)
    
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
    
    ;; Install use-package
    (straight-use-package 'use-package)

    ;; Load the helper package for commands like `straight-x-clean-unused-repos'
   (require 'straight-x)

    ;; Configure use-package to use straight.el by default
    (use-package straight
        :custom (straight-use-package-by-default t))

    ;; straight.el setup of standard packages from the standard repos; elpa, melpa, etc
    (mapc
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
        undo-tree
        hl-todo
        highlight-indent-guides
        hydra
        dash ;; prereq for major-mode-hydra
        s ;; string manip library - prereq for major-mode-hydra
        major-mode-hydra ;; pretty-hydra
        all-the-icons
        all-the-icons-dired
        spaceline-all-the-icons
        dired-single
        dired-subtree
        dired-sidebar
        dired-hide-dotfiles
        git-gutter
        spacegray-theme
        spacemacs-theme
        doom-themes
        spaceline
        rainbow-mode
        rainbow-delimiters
        beacon
        solaire-mode
        pdf-tools
        which-key
        helpful
        ace-window
        vertico
        orderless
        embark
        embark-consult
        consult
        consult-dir
        marginalia
        magit
        libgit
        swap-regions
        format-all
        sh-script ;; make executable after save if editing a script file
        no-littering
        general
        ))
    
    (require 'settings)
    (require 'user-syntax)
    (require 'clean)

    (require 'hydras)
    (require 'keybindings)
    (require 'pkgs-init)

    (require 'hooks)

    (setq gc-cons-threshold 800000)
)
