;;

(set-charset-priority 'unicode)
(setq locale-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))
(set-language-environment "UTF-8")

;; You will most likely need to adjust this font size for your system!
(defvar usr/default-font-size 90)
(defvar usr/default-variable-font-size 110)

(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;;(ignore-errors (set-frame-font "Menlo-12")) ;; mono-spaced font ... ?
(set-face-attribute 'default nil :font "Fira Code Retina" :height usr/default-font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height usr/default-font-size)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height usr/default-variable-font-size :weight 'regular)

;; I like treating - and _ as parts of the word in certain cases
(modify-syntax-entry ?- "w" (standard-syntax-table))
(modify-syntax-entry ?_ "w" (standard-syntax-table))
(modify-syntax-entry ?- "w" emacs-lisp-mode-syntax-table)
(modify-syntax-entry ?_ "w" emacs-lisp-mode-syntax-table)
;;(modify-syntax-entry ?- "w" c-mode-syntax-table)
;;(modify-syntax-entry ?_ "w" c-mode-syntax-table)
(modify-syntax-entry ?- "w" help-mode-syntax-table)
(modify-syntax-entry ?_ "w" help-mode-syntax-table)
(with-eval-after-load 'org
  (modify-syntax-entry ?- "w" org-mode-syntax-table)
  (modify-syntax-entry ?_ "w" org-mode-syntax-table)
)
(with-eval-after-load 'magit
  (modify-syntax-entry ?- "w" magit-mode-syntax-table)
  (modify-syntax-entry ?_ "w" magit-mode-syntax-table)
  (define-key magit-mode-map (kbd "K") 'magit-ls-files)
  ;; ↓ kill all magit buffers associated with this repo when quiting from magit status
  (define-key magit-mode-map [remap magit-mode-bury-buffer]
    (lambda() (interactive) (magit-mode-bury-buffer '(16))))
)
(with-eval-after-load 'helpful
  (modify-syntax-entry ?- "w" helpful-mode-syntax-table)
  (modify-syntax-entry ?_ "w" helpful-mode-syntax-table)
)
(with-eval-after-load 'flycheck
  (modify-syntax-entry ?- "w" flycheck-error-list-mode-syntax-table)
  (modify-syntax-entry ?_ "w" flycheck-error-list-mode-syntax-table)
)

(provide 'user-syntax)
