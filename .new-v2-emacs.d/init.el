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
    (add-to-list 'load-path (concat emacs-private-dir "cfg"))
    (add-to-list 'load-path (concat emacs-private-dir "lib"))

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

    (require 'user-misc-cmds)
    (require 'settings)
    (require 'clean)

    (setq gc-cons-threshold 800000)
)
