;; evil
(use-package evil
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-integration t)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-want-fine-undo t)
  ;;(setq evil-undo-system 'undo-tree)
  :config
  (evil-mode 1)
  ;; much more vim like search interface when ex-mode / is used
  (evil-select-search-module 'evil-search-module 'evil-search)
  (evil-set-initial-state 'message-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)
)

(require 'thingatpt)

(defgroup idle-highlight-in-visible-buffers nil
  "Highlight other occurrences in all visible buffers of the word at point."
  :group 'faces)

(defface idle-highlight-in-visible-buffers
  '((t (:inherit highlight)))
  "Face used to highlight other occurrences of the word at point."
  :group 'idle-highlight-in-visible-buffers)

(defcustom idle-highlight-in-visible-buffers-exceptions '("def" "end")
  "List of words to be excepted from highlighting."
  :group 'idle-highlight-in-visible-buffers
  :type '(repeat string))

(defcustom idle-highlight-in-visible-buffers-idle-time 0.5
  "Time after which to highlight the word at point."
  :group 'idle-highlight-in-visible-buffers
  :type 'float)

(defvar idle-highlight-in-visible-buffers-regexp nil
  "Buffer-local regexp to be idle-highlighted.")

(defvar idle-highlight-in-visible-buffers-global-timer nil
  "Timer to trigger highlighting.")

(defun idle-highlight-in-visible-buffers-buffers-list ()
  "Given a list of buffers, return buffers which are currently visible."
  (let ((buffers '()))
    (walk-windows (lambda (w) (push (window-buffer w) buffers))) buffers))

(defun idle-highlight-in-visible-buffers-unhighlight-word ()
  (interactive)
  "Remove highlighting from all visible buffers."
  (save-window-excursion
    (dolist (buffer (idle-highlight-in-visible-buffers-buffers-list))
      (switch-to-buffer buffer)
      (when idle-highlight-in-visible-buffers-regexp
        (unhighlight-regexp idle-highlight-in-visible-buffers-regexp)))
    (setq idle-highlight-in-visible-buffers-regexp nil)))

;; (defun idle-highlight-in-visible-buffers-highlight-word-at-point ()
;;   (interactive)
;;   "Highlight the word under the point in all visible buffers."
;;   (let* ((target-symbol (symbol-at-point))
;;          (target (symbol-name target-symbol)))
;;     (when (and target-symbol
;;                (not (member target idle-highlight-in-visible-buffers-exceptions)))
;;       (idle-highlight-in-visible-buffers-unhighlight-word)
;;       (save-window-excursion
;;         (dolist (buffer (idle-highlight-in-visible-buffers-buffers-list))
;;           (switch-to-buffer buffer)
;;           (setq idle-highlight-in-visible-buffers-regexp (concat "\\<" (regexp-quote target) "\\>"))
;;           (highlight-regexp idle-highlight-in-visible-buffers-regexp 'idle-highlight-in-visible-buffers))))))

(defun in-visible-buffers-search-highlight-word-at-point ()
  (interactive)
  (let* ((word (evil-find-word t)))
    (save-window-excursion
      (dolist (buffer (idle-highlight-in-visible-buffers-buffers-list))
        (switch-to-buffer buffer)
        (if (search-forward word)
	    (progn (evil-ex-search-word-forward)
		   (evil-ex-search-word-backward))
	  (when (search-backward word)
	      ((progn (evil-ex-search-word-forward)
		   (evil-ex-search-word-backward))))))))
)

(evil-define-key '(normal visual) 'evil-motion-state-map (kbd "*") 'in-visible-buffers-search-highlight-word-at-point)

;; (use-package evil-visualstar
;;   :defer 1
;;   :after evil
;;   :config
;;   (global-evil-visualstar-mode t)
;; )

;; (with-eval-after-load 'evil-visualstar
;;   ;; (evil-define-key '(normal) 'global  (kbd ", SPC") ')
;; )

(use-package evil-anzu
  :after evil
)

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init)
)

(use-package evil-mc
  :defer 1
  :after evil
  :config
  (global-evil-mc-mode 1)
)

;; for incr/decr numbers in various patterns; dec, oct, hex, bin
(use-package evil-numbers
  :defer t
  :after evil
)

(use-package evil-nerd-commenter
  :defer t
  :after evil
  :bind ("C-;" . evilnc-comment-or-uncomment-lines)
)

(with-eval-after-load 'evil-mc
  (setq evil-mc-cursor-variables
        (mapcar
         (lambda (s)
           (remove 'register-alist
                   (remove 'evil-markers-alist
                           (remove evil-was-yanked-without-register s))))
         evil-mc-cursor-variables))
  ;; Redefine this function to fix cursor misalignment issues.
  ;; e.g. With multiple cursors, visually select one character and change.
  ;;      With the original `evil-mc-execute-evil-change' the fake cursors would jump one
  ;;      character to the left, incorrectly.
  (defun evil-mc-execute-evil-change ()
    "Execute an `evil-change' comand."
    (let ((point (point)))
      (evil-with-state normal
        (unless (eq point (point-at-bol))
          (evil-forward-char 1 nil t)) ; Perhaps this behavior depends on `evil-move-cursor-back'?
        (evil-mc-execute-with-region-or-macro 'evil-change)
        (evil-maybe-remove-spaces nil))))
)

;; makes certain Vim like operator-state sequences operate on entire word
;; yw -> yiw, dw -> diw
;; this will only apply to the below specified commands; evil-yank/delete/change
;; source : https://stackoverflow.com/questions/37238920/key-mapping-in-evil-mode-emacs
(defun usr/evil-motion-range (orig-fun &rest args)
  (if (not (memq this-command '(evil-yank evil-delete)))
      (apply orig-fun args)
    (let* ((orig-keymap evil-operator-state-local-map)
           (evil-operator-state-local-map (copy-keymap orig-keymap)))
      (define-key evil-operator-state-local-map "w" "iw")
      (apply orig-fun args))))

(with-eval-after-load 'evil
  (advice-add 'evil-operator-range :around #'usr/evil-motion-range)
)

(provide 'evil-init)
