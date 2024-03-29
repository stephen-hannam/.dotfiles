;; vertico, orderless, consult, embark, marginalia, corfu, cape, 'skeletons'?
;; embark-consult
;; embark mappings - bookmark, buffer, command, defun, email, expression, face, file, vc, function, general, consult-search, heading, identifier, kill-ring, library, package, ...
;; https://karthinks.com/software/fifteen-ways-to-use-embark/

(use-package vertico
  :custom
  (vertico-cycle t)
  :custom-face
  (vertico-current ((t (:background "#3a3f5a"))))
  :config
  (vertico-mode)
)

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

(use-package marginalia
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :config
  (marginalia-mode)
  :bind
  (("<tab>" . minibuffer-complete))
)

(use-package orderless
  :custom
  (completion-styles '(substring partial-completion orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles . (partial-completion)))))
)

;;(eval-when-compile
;;  (defmacro my/embark-ace-action (fn)
;;    `(defun ,(intern (concat "my/embark-ace-" (symbol-name fn))) ()
;;       (interactive)
;;       (with-demoted-errors "%s"
;;         (require 'ace-window)
;;         (let ((aw-dispatch-always t))
;;           (aw-switch-to-window (aw-select nil))
;;           (call-interactively (symbol-function ',fn)))))))
;;
;;(eval-when-compile
;;  (defmacro my/embark-split-action (fn split-type)
;;    `(defun ,(intern (concat "my/embark-"
;;                             (symbol-name fn)
;;                             "-"
;;                             (car (last  (split-string
;;                                          (symbol-name split-type) "-"))))) ()
;;       (interactive)
;;       (funcall #',split-type)
;;       (call-interactively #',fn))))
;;
;;;; NOTE: embark, embark-consult: starting config from github
;;(use-package embark
;;  :bind
;;  (("C-." . embark-act)         ;; pick some comfortable binding
;;   ("C-'" . embark-dwim)        ;; good alternative: M-.
;;   ("C-M-e" . embark-export)    ;; for exercises on 15 ways to use embark
;;   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
;;
;;  :init
;;  ;; Optionally replace the key help with a completing-read interface
;;  (setq prefix-help-command #'embark-prefix-help-command)
;;
;;  :config
;;  ;; Hide the mode line of the Embark live/completions buffers
;;  (add-to-list 'display-buffer-alist
;;               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
;;                 nil
;;                 (window-parameters (mode-line-format . none))))
;;
;;  (define-key embark-file-map     (kbd "o") (my/embark-ace-action find-file))
;;  (define-key embark-buffer-map   (kbd "o") (my/embark-ace-action switch-to-buffer))
;;  (define-key embark-bookmark-map (kbd "o") (my/embark-ace-action bookmark-jump))
;;
;;  (define-key embark-file-map     (kbd "2") (my/embark-split-action find-file split-window-below))
;;  (define-key embark-buffer-map   (kbd "2") (my/embark-split-action switch-to-buffer split-window-below))
;;  (define-key embark-bookmark-map (kbd "2") (my/embark-split-action bookmark-jump split-window-below))
;;
;;  (define-key embark-file-map     (kbd "3") (my/embark-split-action find-file split-window-right))
;;  (define-key embark-buffer-map   (kbd "3") (my/embark-split-action switch-to-buffer split-window-right))
;;  (define-key embark-bookmark-map (kbd "3") (my/embark-split-action bookmark-jump split-window-right))
;;)
;;
;;;; Consult users will also want the embark-consult package.
;;(use-package embark-consult
;;  :after (embark consult)
;;  :demand t ; only necessary if you have the hook below
;;  ;; if you want to have consult previews as you move around an
;;  ;; auto-updating embark collect buffer
;;  :hook
;;  (embark-collect-mode . consult-preview-at-point-mode)
;;)
;;
;;;; Example configuration for Consult
;;(use-package consult
;;  ;; Replace bindings. Lazily loaded due by `use-package'.
;;  :bind (;; C-c bindings (mode-specific-map)
;;         ("C-c h" . consult-history)
;;         ("C-c m" . consult-mode-command)
;;         ("C-c k" . consult-kmacro)
;;         ;; C-x bindings (ctl-x-map)
;;         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
;;         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
;;         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
;;         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
;;         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
;;         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
;;         ;; Custom M-# bindings for fast register access
;;         ("M-#" . consult-register-load)
;;         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
;;         ("C-M-#" . consult-register)
;;         ;; Other custom bindings
;;         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
;;         ("<help> a" . consult-apropos)            ;; orig. apropos-command
;;         ;; M-g bindings (goto-map)
;;         ("M-g e" . consult-compile-error)
;;         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
;;         ("M-g g" . consult-goto-line)             ;; orig. goto-line
;;         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
;;         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
;;         ("M-g m" . consult-mark)
;;         ("M-g k" . consult-global-mark)
;;         ("M-g i" . consult-imenu)
;;         ("M-g I" . consult-imenu-multi)
;;         ;; M-s bindings (search-map)
;;         ("M-s d" . consult-find)
;;         ("M-s D" . consult-locate)
;;         ("M-s g" . consult-grep)
;;         ("M-s G" . consult-git-grep)
;;         ("M-s r" . consult-ripgrep)
;;         ("M-s l" . consult-line)
;;         ("M-s L" . consult-line-multi)
;;         ("M-s m" . consult-multi-occur)
;;         ("M-s k" . consult-keep-lines)
;;         ("M-s u" . consult-focus-lines)
;;         ;; Isearch integration
;;         ("M-s e" . consult-isearch-history)
;;         :map isearch-mode-map
;;         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
;;         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
;;         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
;;         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
;;         ;; Minibuffer history
;;         :map minibuffer-local-map
;;         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
;;         ("M-r" . consult-history))                ;; orig. previous-matching-history-element
;;
;;  ;; Enable automatic preview at point in the *Completions* buffer. This is
;;  ;; relevant when you use the default completion UI.
;;  :hook (completion-list-mode . consult-preview-at-point-mode)
;;
;;  ;; The :init configuration is always executed (Not lazy)
;;  :init
;;
;;  ;; Optionally configure the register formatting. This improves the register
;;  ;; preview for `consult-register', `consult-register-load',
;;  ;; `consult-register-store' and the Emacs built-ins.
;;  (setq register-preview-delay 0.5
;;        register-preview-function #'consult-register-format)
;;
;;  ;; Optionally tweak the register preview window.
;;  ;; This adds thin lines, sorting and hides the mode line of the window.
;;  (advice-add #'register-preview :override #'consult-register-window)
;;
;;  ;; Use Consult to select xref locations with preview
;;  (setq xref-show-xrefs-function #'consult-xref
;;        xref-show-definitions-function #'consult-xref)
;;
;;  ;; Configure other variables and modes in the :config section,
;;  ;; after lazily loading the package. 
;;  :config
;;
;;  ;; Optionally configure preview. The default value
;;  ;; is 'any, such that any key triggers the preview.
;;  ;; (setq consult-preview-key 'any)
;;  ;; (setq consult-preview-key (kbd "M-."))
;;  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
;;  ;; For some commands and buffer sources it is useful to configure the
;;  ;; :preview-key on a per-command basis using the `consult-customize' macro.
;;  (consult-customize
;;   consult-theme
;;   :preview-key '(:debounce 0.2 any)
;;   consult-ripgrep consult-git-grep consult-grep
;;   consult-bookmark consult-recent-file consult-xref
;;   consult--source-bookmark consult--source-recent-file
;;   consult--source-project-recent-file
;;   :preview-key (kbd "M-."))
;;
;;  ;; Optionally configure the narrowing key.
;;  ;; Both < and C-+ work reasonably well.
;;  (setq consult-narrow-key "<") ;; (kbd "C-+")
;;
;;  ;; Optionally make narrowing help available in the minibuffer.
;;  ;; You may want to use `embark-prefix-help-command' or which-key instead.
;;  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)
;;
;;  ;; By default `consult-project-function' uses `project-root' from project.el.
;;  ;; Optionally configure a different project root function.
;;  ;; There are multiple reasonable alternatives to chose from.
;;  ;;;; 1. project.el (the default)
;;  ;; (setq consult-project-function #'consult--default-project--function)
;;  ;;;; 2. projectile.el (projectile-project-root)
;;  ;; (autoload 'projectile-project-root "projectile")
;;  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
;;  ;;;; 3. vc.el (vc-root-dir)
;;  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
;;  ;;;; 4. locate-dominating-file
;;  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
;;)
;;
;;(use-package consult-dir
;;  :bind (("C-x C-d" . consult-dir)
;;         :map vertico-map
;;         ("C-x C-d" . consult-dir)
;;         ("C-x C-j" . consult-dir-jump-file))
;;)

(provide 'completions-init)
