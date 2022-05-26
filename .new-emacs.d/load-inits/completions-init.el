;; vertico, orderless, consult, embark, marginalia, corfu, cape, 'skeletons'?
;; embark-consult
;; embark mappings - bookmark, buffer, command, defun, email, expression, face, file, vc, function, general, consult-search, heading, identifier, kill-ring, library, package, ...
;; https://karthinks.com/software/fifteen-ways-to-use-embark/

(use-package vertico
  :custom
  (vertico-cycle t)
  :custom-face
  (vertico-current ((t (:background "#3a3f5a"))))
  :init
  (vertico-mode)
  )

(use-package marginalia
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode)
  )

(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion)))))
  )

(use-package marginalia
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode)
  )

(provide 'completions-init)
