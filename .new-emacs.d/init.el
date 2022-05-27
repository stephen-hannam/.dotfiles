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

    (add-to-list 'load-path (concat emacs-private-dir "el-files"))
    (add-to-list 'load-path (concat emacs-private-dir "load-inits"))

    (require 'cl)
    (require 'move-border)
    (require 'tabularize)
    (require 'misc-cmds)
    (require 'user-misc-cmds)
    (require 'text-manips)
    (require 'interaction-log)
    
    (require 'settings)
    (require 'hooks)

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
        highlight-indent-guides
        idle-highlight-in-visible-buffers-mode
        hydra
        major-mode-hydra ;; pretty-hydra
        all-the-icons
        all-the-icons-dired
        spaceline-all-the-icons
        dired-single
        dired-subtree
        dired-sidebar
        dired-hide-dotfiles
        spaceline
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
    
    (require 'pkgs-init)
    ;;(require 'keybindings-init)
    ;;(require 'hydras-init)

    (setq gc-cons-threshold 800000)
)
