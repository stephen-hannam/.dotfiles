(("stemacs" . ((user-emacs-directory . "~/.stemacs.d")
	           (server-name . "stemacs")
	           (env . (("EMACSDIR" . "~/.stemacs.d")))
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

("prelude" . ((user-emacs-directory . "~/prelude-emacs")
              (server-name . "preludemacs")
              (env . (("EMACSDIR" . "~/.prelude-emacs.d")))
              ))

("modi" . ((user-emacs-directory . "~/.modi-emacs.d")
           (server-name . "modimacs")
           (env . (("EMACSDIR" . "~/.modi-emacs.d")))
           ))

;; for experimenting, just a basic vanilla config
("exp" . ((user-emacs-directory . "~/.exp-emacs.d")
          (server-name . "expmacs")
          (env . (("EMACSDIR" . "~/.exp-emacs.d")))
          ))

("new" . ((user-emacs-directory . "~/.new-emacs.d")
          (server-name . "newmacs")
          (env . (("EMACSDIR" . "~/.new-emacs.d")))
          ))
)
