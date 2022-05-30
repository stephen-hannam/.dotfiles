(global-set-key (kbd "C-h .") nil)
(global-set-key (kbd "M-.") nil)
(global-set-key (kbd "<pause>") nil)

(define-key global-map [remap quit-window] 'delete-window-maybe-kill-buffer)

;; free up C-u for other purposes
(global-set-key (kbd "C-M-u") 'universal-argument)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-h q") #'helpful-kill-buffers)

;; Navigate through buffers
(global-set-key (kbd "M-[") 'previous-buffer)
(global-set-key (kbd "M-]") 'next-buffer)
(global-set-key (kbd "<f12>") 'swiper)

(defun set-extended-char-shortcuts ()
  (interactive)
  (progn
    ;; just in case the OS level key-swap disappears from the parent shell session
    (define-key key-translation-map (kbd "<capslock>") (kbd "<escape>"))
    ;; set keys to insert math symbol
    (define-key key-translation-map (kbd "<f9> a") (kbd "α"))
    (define-key key-translation-map (kbd "<f9> b") (kbd "β"))
    (define-key key-translation-map (kbd "<f9> g") (kbd "γ"))
    (define-key key-translation-map (kbd "<f9> d") (kbd "δ"))
    (define-key key-translation-map (kbd "<f9> e") (kbd "ε"))
    (define-key key-translation-map (kbd "<f9> z") (kbd "ζ"))
    (define-key key-translation-map (kbd "<f9> h") (kbd "η"))
    (define-key key-translation-map (kbd "<f9> q") (kbd "θ"))
    (define-key key-translation-map (kbd "<f9> i") (kbd "ι"))
    (define-key key-translation-map (kbd "<f9> k") (kbd "κ"))
    (define-key key-translation-map (kbd "<f9> l") (kbd "λ"))
    (define-key key-translation-map (kbd "<f9> m") (kbd "μ"))
    (define-key key-translation-map (kbd "<f9> n") (kbd "ν")) 
    (define-key key-translation-map (kbd "<f9> x") (kbd "ξ"))
    (define-key key-translation-map (kbd "<f9> p") (kbd "π"))
    (define-key key-translation-map (kbd "<f9> r") (kbd "ρ"))
    (define-key key-translation-map (kbd "<f9> s") (kbd "σ"))
    (define-key key-translation-map (kbd "<f9> t") (kbd "τ"))
    (define-key key-translation-map (kbd "<f9> u") (kbd "υ"))
    (define-key key-translation-map (kbd "<f9> f") (kbd "ϕ"))
    (define-key key-translation-map (kbd "<f9> j") (kbd "φ"))
    (define-key key-translation-map (kbd "<f9> c") (kbd "χ"))
    (define-key key-translation-map (kbd "<f9> y") (kbd "ψ"))
    (define-key key-translation-map (kbd "<f9> w") (kbd "ω"))
    (define-key key-translation-map (kbd "<f9> G") (kbd "Γ"))
    (define-key key-translation-map (kbd "<f9> D") (kbd "Δ"))
    (define-key key-translation-map (kbd "<f9> Q") (kbd "Θ"))
    (define-key key-translation-map (kbd "<f9> L") (kbd "Λ"))
    (define-key key-translation-map (kbd "<f9> X") (kbd "Ξ"))
    (define-key key-translation-map (kbd "<f9> P") (kbd "Π"))
    (define-key key-translation-map (kbd "<f9> S") (kbd "Σ"))
    (define-key key-translation-map (kbd "<f9> Y") (kbd "Ψ"))
    (define-key key-translation-map (kbd "<f9> W") (kbd "Ω"))
    (define-key key-translation-map (kbd "<f9> N") (kbd "∇"))
    (define-key key-translation-map (kbd "<f9> <down>") (kbd "↓"))
    (define-key key-translation-map (kbd "<f9> <left>") (kbd "←"))
    (define-key key-translation-map (kbd "<f9> <right>") (kbd "→"))
    (define-key key-translation-map (kbd "<f9> <up>") (kbd "↑"))
    (define-key key-translation-map (kbd "<f9> 8") (kbd "★"))
    (define-key key-translation-map (kbd "<f9> 6") (kbd "◆"))
    (define-key key-translation-map (kbd "<f9> I") (kbd "∞"))
    )
)

(add-hook 'after-init-hook #'set-extended-char-shortcuts)

(define-key map (kbd "M-<up>") #'windmove-up)
(define-key map (kbd "M-<down>") #'windmove-down)
(define-key map (kbd "M-<left>") #'windmove-left)
(define-key map (kbd "M-<right>") #'windmove-right)
(define-key map (kbd "M-S-<up>") #'move-border-up)
(define-key map (kbd "M-S-<down>") #'move-border-down)
(define-key map (kbd "M-S-<left>") #'move-border-left)
(define-key map (kbd "M-S-<right>") #'move-border-right)
(define-key map (kbd "C-<escape>") #'abort-recursive-edit)
(define-key map (kbd "C-s") #'swap-regions)
(define-key map (kbd "C-q") #'kill-current-buffer)
(define-key map (kbd "M-q") #'quoted-insert)
;;(define-key map (kbd "C-<left>") 'srh/sp-backward-slurp-maybe)
;;(define-key map (kbd "C-<right>") 'srh/sp-forward-slurp-maybe)
;;(define-key map (kbd "C-S-<left>") 'srh/sp-backward-barf-maybe)
;;(define-key map (kbd "C-S-<right>") 'srh/sp-forward-barf-maybe)

(defun usr/evil-motion-range (orig-fun &rest args)
    "makes certain Vim like operator-state sequences operate on entire word
    yw -> yiw, dw -> diw
    this will only apply to the below specified commands; evil-yank/delete/change
    source : https://stackoverflow.com/questions/37238920/key-mapping-in-evil-mode-emacs"
  (if (not (memq this-command '(evil-yank evil-delete)))
      (apply orig-fun args)
    (let* ((orig-keymap evil-operator-state-local-map)
           (evil-operator-state-local-map (copy-keymap orig-keymap)))
      (define-key evil-operator-state-local-map "w" "iw")
      (apply orig-fun args))))

(defun usr/delete-window-maybe-kill-buffer-maybe-delete-frame ()
  (interactive)
  (if (eq (count-windows) 1)
      (evil-ex-call-command nil "quit" nil)
    (delete-window-maybe-kill-buffer))
)

(with-eval-after-load 'evil
  ;;(unbind-key "C-." 'evil-normal-state-map) -- ? solved in Gnome by Alt-F2 -> ibus-setup
  (evil-define-key '(normal) 'global  (kbd "M-.") #'helpful-at-point)
  (evil-define-key '(normal visual insert) 'global  (kbd "M-DEL") 'sp-unwrap-sexp)
  (evil-define-key '(normal visual) 'global (kbd "C-e") 'exit-recursive-edit)
  (evil-define-key '(insert) 'global (kbd "C-g") 'evil-normal-state)
  (evil-define-key '(normal) 'global (kbd "r") 'evil-replace-state)
  (evil-define-key '(normal) 'global (kbd "<left>") 'evil-backward-word-begin)
  (evil-define-key '(normal) 'global (kbd "<right>") 'evil-forward-word-end)
  (evil-define-key '(normal) 'global (kbd "S-<up>") 'evil-backward-paragraph)
  (evil-define-key '(normal) 'global (kbd "S-<down>") 'evil-forward-paragraph)
  (evil-define-key '(normal visual insert) 'global (kbd "C-<up>")
    (lambda() (interactive) (scroll-other-window-down 1)))
  (evil-define-key '(normal visual insert) 'global (kbd "C-<down>")
    (lambda() (interactive) (scroll-other-window-down -1)))
  (evil-define-key '(normal) 'global (kbd "RET") (lambda() (interactive) (evil-insert-newline-below)))
  ;; :q should kill the current buffer rather than quitting emacs entirely
  (evil-ex-define-cmd "q" 'usr/delete-window-maybe-kill-buffer-maybe-delete-frame)
  (evil-ex-define-cmd "aq" 'kill-other-buffers)
  ;; Need to type out :quit to close emacs
  (evil-ex-define-cmd "quit" 'evil-quit)
  (global-undo-tree-mode)
  (turn-on-undo-tree-mode)
  (advice-add 'evil-operator-range :around #'usr/evil-motion-range)

  (setq evil-emacs-state-cursor '("#81a2be" box))
  (setq evil-normal-state-cursor '("#81a2be" box))
  (setq evil-visual-state-cursor '("orange" box))
  (setq evil-insert-state-cursor '("green" bar))
  (setq evil-replace-state-cursor '("red" box))
  (setq evil-operator-state-cursor '("red" hollow))
  (evil-define-key '(normal) 'global  (kbd "M-.") #'helpful-at-point)
  (evil-define-key '(normal visual insert) 'global  (kbd "M-DEL") 'sp-unwrap-sexp)
  (evil-define-key '(normal visual) 'global (kbd "C-e") 'exit-recursive-edit)
  (evil-define-key '(insert) 'global (kbd "C-g") 'evil-normal-state)
  (evil-define-key '(normal) 'global (kbd "r") 'evil-replace-state)
  (evil-define-key '(normal) 'global (kbd "<left>") 'evil-backward-word-begin)
  (evil-define-key '(normal) 'global (kbd "<right>") 'evil-forward-word-end)
  (evil-define-key '(normal) 'global (kbd "S-<up>") 'evil-backward-paragraph)
  (evil-define-key '(normal) 'global (kbd "S-<down>") 'evil-forward-paragraph)
  (evil-define-key '(normal visual insert) 'global (kbd "C-<up>")
    (lambda() (interactive) (scroll-other-window-down 1)))
  (evil-define-key '(normal visual insert) 'global (kbd "C-<down>")
    (lambda() (interactive) (scroll-other-window-down -1)))
  (evil-define-key '(normal) 'global (kbd "RET") (lambda() (interactive) (evil-insert-newline-below)))
  ;; :q should kill the current buffer rather than quitting emacs entirely
  (evil-ex-define-cmd "q" 'usr/delete-window-maybe-kill-buffer-maybe-delete-frame)
  (evil-ex-define-cmd "aq" 'kill-other-buffers)
  ;; Need to type out :quit to close emacs
  (evil-ex-define-cmd "quit" 'evil-quit)
)

(with-eval-after-load 'evil-nerd-commenter
  (evil-define-key '(normal visual insert) 'global  (kbd "M-/") 'evilnc-comment-or-uncomment-lines)
)

(with-eval-after-load 'evil-numbers
  (evil-define-key '(normal visual) 'global (kbd "+") 'evil-numbers/inc-at-pt-incremental)
  (evil-define-key '(normal visual) 'global (kbd "-") 'evil-numbers/dec-at-pt-incremental)
  (evil-define-key '(normal visual) 'global (kbd "C-+") 'evil-numbers/inc-at-pt)
  (evil-define-key '(normal visual) 'global (kbd "C--") 'evil-numbers/dec-at-pt)
)

(with-eval-after-load 'evil-mc
  (evil-define-key '(normal visual) 'global (kbd "R") 'evil-mc-undo-all-cursors)
  (evil-define-key '(normal visual) 'global (kbd "!") 'usr/mc-toggle-cursors)
)

(with-eval-after-load 'evil-collection
  (evil-collection-define-key 'normal 'dired-mode-map
    (kbd "<kp-0>") 'dired-up-directory)
  (evil-collection-define-key 'normal 'dired-mode-map
    (kbd "o") 'dired-find-file-other-window)
  (evil-collection-define-key 'normal 'dired-mode-map
    (kbd "e") 'dired-create-empty-file)
  (evil-collection-define-key 'normal 'dired-mode-map
    (kbd "G") 'dired-do-chgrp)
  (evil-collection-define-key 'normal 'dired-mode-map
    (kbd "k") '(lambda() (interactive) (dired-do-kill-lines 1)))
  (evil-collection-define-key 'normal 'dired-mode-map
    (kbd "F") '(lambda() (interactive) (dired-do-find-marked-files)))
  (evil-collection-define-key 'normal 'image-dired-thumbnail-mode-map
    (kbd "<right>") 'image-dired-forward-image)
  (evil-collection-define-key 'normal 'image-dired-thumbnail-mode-map
    (kbd "<left>") 'image-dired-backward-image)
)

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "M-S-<up>") nil)
  (define-key org-mode-map (kbd "M-S-<down>") nil)
  (define-key org-mode-map (kbd "M-S-<left>") nil)
  (define-key org-mode-map (kbd "M-S-<right>") nil)
  (define-key org-mode-map (kbd "M-<up>") nil)
  (define-key org-mode-map (kbd "M-<down>") nil)
  (define-key org-mode-map (kbd "M-<left>") nil)
  (define-key org-mode-map (kbd "M-<right>") nil)
  (define-key org-mode-map [C-S-up] 'org-shiftmetaup)
  (define-key org-mode-map [C-S-down] 'org-shiftmetadown)
  (define-key org-mode-map [C-S-right] 'org-shiftmetaright)
  (define-key org-mode-map [C-S-left] 'org-shiftmetaleft)
  (define-key org-mode-map [C-up] 'org-metaup)
  (define-key org-mode-map [C-down] 'org-metadown)
  (define-key org-mode-map [C-right] 'org-metaright)
  (define-key org-mode-map [C-left] 'org-metaleft)
  (define-key org-mode-map (kbd "M-\\") 'org-toggle-checkbox)
) 

(with-eval-after-load 'evil-collection
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-hide-dotfiles-mode)
)

(with-eval-after-load 'dired-subtree
  (bind-key "<tab>" #'dired-subtree-toggle dired-mode-map)
  (bind-key "<backtab>" #'dired-subtree-cycle dired-mode-map)
)

;; I like treating - and _ as parts of the word in certain cases
(modify-syntax-entry ?- "w" (standard-syntax-table))
(modify-syntax-entry ?_ "w" (standard-syntax-table))
(modify-syntax-entry ?- "w" emacs-lisp-mode-syntax-table)
(modify-syntax-entry ?_ "w" emacs-lisp-mode-syntax-table)
(modify-syntax-entry ?- "w" c-mode-syntax-table)
(modify-syntax-entry ?_ "w" c-mode-syntax-table)
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

;; TODO: make use of double-tap bindings
;; NOTE: I think key-chord let's you do the same thing ... 
;; ... (key-chord-define-global ',,' 'indent-for-comment)   OR
;; ... (key-chord-define-global 'QQ' "The ")
;; ^^^ called ONE-key-chord (max 1/3 sec btw presses)
(setq doubletap-flag nil)

(defun doubletap (doubletap-key1 doubletap-key2 doubletap-wait doubletap-function)
  (setq doubletap-flag 't)
   (let ((doubletap-event (read-event nil nil doubletap-wait)))
     (if doubletap-event
       (if (equal doubletap-event doubletap-key2)
         (progn (setq doubletap-flag nil) (funcall doubletap-function))
       (setq unread-command-events
             (append (list doubletap-key1 doubletap-event) unread-command-events)) )
     (setq unread-command-events
           (append (list doubletap-key1) unread-command-events)) )))

;;(defun doubletap-j-j-godmodeall () (interactive)
;;       (if doubletap-flag
;;         (progn (setq doubletap-flag nil) (funcall 'self-insert-command 1))
;;         (doubletap 106 106 0.3 'god-mode-all)))
;;
;;(global-set-key (kbd "j") 'doubletap-j-j-godmodeall)

(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line] 'smarter-move-beginning-of-line)

;; remap home to `smarter-move-beginning-of-line'
(global-set-key [remap evil-beginning-of-line] 'smarter-move-beginning-of-line)
