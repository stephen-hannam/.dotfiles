(("stemacs" . ((user-emacs-directory . "~/.stemacs.d")
	           (server-name . "stemacs")
	           (env . (("STEMACSDIR" . "~/.stemacs.d")))
	       ))

;; emacs distribution: DOOM-emacs
("doom" . ((user-emacs-directory . "~/doom-emacs")
	       (server-name . "doom")
	       (env . (("DOOMDIR" . "~/.doom.d")))
	     ))

 ("spacemacs" . ((user-emacs-directory . "~/spacemacs")
                 (server-name . "spacemacs")
                 (env . (("SPACEMACSDIR" . "~/.spacemacs.d")))
                 ))

("prelude" . ((user-emacs-directory . "~/prelude-emacs")))

("modi" . ((user-emacs-directory . "~/.modi-emacs.d")))

;; for experimenting, just a basic vanilla config
("exp" . ((user-emacs-directory . "~/.exp-emacs.d")))

("new" . ((user-emacs-directory . "~/.new-emacs.d")))
)
