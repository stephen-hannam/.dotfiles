;; hooks.el

(add-hook 'emacs-lisp-mode-hook (lambda() (setq mode-name " ξ ")))

(add-hook 'emacs-startup-hook
        (lambda ()
          (message "Emacs ready in %s with %d garbage collections."
                   (format "%.2f seconds"
                           (float-time
                            (time-subtract after-init-time before-init-time)))
                   gcs-done)))

;; disable line numbers for some modes
(dolist (mode '(
                org-mode-hook
		        term-mode-hook
                help-mode-hook
                special-mode-hook
                dired-mode-hook
                helpful-mode-hook
		        shell-mode-hook
                vterm-mode-hook
                treemacs-mode-hook
                undo-tree-visualizer-mode-hook
		        eshell-mode-hook
                org-indent-mode-hook
                ielm-mode-hook
                comint-mode-hook
                ))
  (add-hook mode (lambda() (display-line-numbers-mode 0)))
  )

;; enable line high-lighting only for some modes
(dolist (mode '(
                prog-mode-hook
		        text-mode-hook
                ))
  (add-hook mode (lambda() (hl-line-mode t)))
  )

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  :init-value t
  :lighter " my-keys")
(my-keys-minor-mode 1)

(defun my-keys-have-priority (_file)
  "Try to ensure that my keybindings retain priority over other minor modes.
Called via the `after-load-functions' special hook."
  (unless (eq (caar minor-mode-map-alist) 'my-keys-minor-mode)
    (let ((mykeys (assq 'my-keys-minor-mode minor-mode-map-alist)))
      (assq-delete-all 'my-keys-minor-mode minor-mode-map-alist)
      (add-to-list 'minor-mode-map-alist mykeys))))
(add-hook 'after-load-functions 'my-keys-have-priority)

(defun common-init-calls ()
  (set-face-attribute 'default nil
                      :font  "Fira Code Retina"
                      :weight 'normal
                      :height usr/default-font-size)
  )

(defun client-frame-init ()
  (common-init-calls)
  (message "emacs client post-frame init complete!")
  )

(defun standalone-frame-init ()
  (common-init-calls)
  ;;(load-theme 'doom-tomorrow-night t)
  (load-theme 'spacemacs-dark t)
  (message "emacs standalone post-frame init complete!")
  )

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (select-frame frame)
                (client-frame-init)))
  (standalone-frame-init)
  )

(provide 'hooks)
