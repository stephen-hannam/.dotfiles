(use-package term-keys
  :config
  (term-keys-mode t)
)

(defun generate-kitty-term-keys ()
  (interactive)
  (when term-keys-mode
    (progn
     (require 'term-keys-kitty)   
     (with-temp-buffer
       (insert (term-keys/kitty-conf))
       (write-region (point-min) (point-max) "~/.config/kitty/kitty-for-term-keys.conf")
     )
   )
  )
)

(provide 'try-term-keys)
