(("stemacs" . ((user-emacs-directory . "~/.stemacs.d")
	           (server-name . "stemacs")
	           (env . (("STEMACSDIR" . "~/.stemacs.d")))
	       ))

;; emacs distribution: DOOM-emacs
("doom" . ((user-emacs-directory . "~/doom-emacs")
	       (server-name . "doom")
	       (env . (("DOOMDIR" . "~/.doom.d")))
	     ))

("modi" . ((user-emacs-directory . "~/.modi-emacs.d")))

;; for experimenting, just a basic vanilla config
("exp" . ((user-emacs-directory . "~/.exp-emacs.d")))

("new" . ((user-emacs-directory . "~/.new-emacs.d")))
)
