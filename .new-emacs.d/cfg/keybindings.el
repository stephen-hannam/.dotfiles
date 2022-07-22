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

(defvar my-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-<up>") #'windmove-up)
    (define-key map (kbd "M-<down>") #'windmove-down)
    (define-key map (kbd "M-<left>") #'windmove-left)
    (define-key map (kbd "M-<right>") #'windmove-right)
    (define-key map (kbd "M-S-<up>") #'move-border-up)
    (define-key map (kbd "M-S-<down>") #'move-border-down)
    (define-key map (kbd "M-S-<left>") #'move-border-left)
    (define-key map (kbd "M-S-<right>") #'move-border-right)
    (define-key map (kbd "C-<escape>") #'abort-recursive-edit)
    ;;(define-key map (kbd "C-s") #'swap-regions)
    (define-key map (kbd "C-q") #'kill-current-buffer)
    (define-key map (kbd "M-q") #'quoted-insert)
    ;;(define-key map (kbd "C-<left>") 'srh/sp-backward-slurp-maybe)
    ;;(define-key map (kbd "C-<right>") 'srh/sp-forward-slurp-maybe)
    ;;(define-key map (kbd "C-S-<left>") 'srh/sp-backward-barf-maybe)
    ;;(define-key map (kbd "C-S-<right>") 'srh/sp-forward-barf-maybe)
    map)
)

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  :init-value t
  :lighter " my-keys"
)

(my-keys-minor-mode 1)

(defun my-keys-have-priority (_file)
  "Try to ensure that my keybindings retain priority over other minor modes.
Called via the `after-load-functions' special hook."
  (unless (eq (caar minor-mode-map-alist) 'my-keys-minor-mode)
    (let ((mykeys (assq 'my-keys-minor-mode minor-mode-map-alist)))
      (assq-delete-all 'my-keys-minor-mode minor-mode-map-alist)
      (add-to-list 'minor-mode-map-alist mykeys)))
)

(add-hook 'after-load-functions 'my-keys-have-priority)

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

(provide 'keybindings)
