(setq gc-cons-threshold (* 50 1024 1024)
      read-process-output-max (* 1024 1024)
      lexical-binding t)

(defun srh/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'srh/display-startup-time)

(defun srh/add-to-list-multiple (list to-add)
  "Add multiple items to LIST.
Allows for adding a sequence of items to the same list, rather
than having to call `add-to-list' multiple times."
  (interactive)
  (dolist (item to-add)
    (add-to-list list item)))

(custom-set-variables
 '(default-frame-alist '((fullscreen . maximized)))) ;; start maximized

;; thanks but no thanks
(setq inhibit-startup-message t
      initial-scratch-message nil
      sentence-end-double-space nil
      ring-bell-function 'ignore
      use-dialog-box nil
      case-fold-search nil
      compilation-scroll-output t
      load-prefer-newer t
      help-window-select t)

;; makes help buffer use the same window every time
(setq display-buffer-alist
      `((,(rx bos (or "*Apropos*" "*Help*" "*helpful" "*info*" "*Summary*") (0+ not-newline))
         (display-buffer-reuse-mode-window display-buffer-pop-up-window)
         (mode apropos-mode help-mode helpful-mode Info-mode Man-mode))))

(setq make-backup-files nil
      auto-save-default nil
      create-lockfiles nil
      custom-file null-device)

(menu-bar-mode -1) ; Disable menu bar
(scroll-bar-mode -1) ; Disable visual scrollbar
(tool-bar-mode -1) ; Disable toolbar
(tooltip-mode -1) ; Disable tooltips
(set-fringe-mode 10) ; Give some breathing room

;; FIXME: save-places may not be working in daemon mode
(save-place-mode 1)
(setq save-place-file (locate-user-emacs-file "places" ".emacs-places"))

(column-number-mode)
(global-display-line-numbers-mode t)
(delete-selection-mode t)
(show-paren-mode t)

(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)
(srh/add-to-list-multiple 'auto-mode-alist '(
                                             ("\\.*rc$" . conf-unix-mode)
                                             ("\\.bash*" . conf-unix-mode)
                                             ("/etc/**/bash*" . conf-unix-mode)
                                             ("~/\\.*" . conf-unix-mode)
                                             ))
(global-prettify-symbols-mode 1) ;; shows "lambda" as "λ"
(defalias 'yes-or-no-p 'y-or-n-p)
(add-hook 'emacs-lisp-mode-hook (lambda() (setq mode-name " ξ ")))

(setq show-paren-when-point-inside-paren t
      show-paren-delay 0
      show-paren-style 'mixed
      windmove-wrap-around t)

(setq-default indent-tabs-mode nil
              tab-always-indent 'complete
              tab-width 4)

(setq indent-tabs-mode nil
      tab-always-indent 'complete
      tab-width 4)

(set-charset-priority 'unicode)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))
(set-language-environment "UTF-8")

(setq display-time-24hr-format t)
(display-time-mode 1)

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

(defun my-insert-tab-char ()
  "insert a tab char. (ASCII 9, \t) -- or n spaces"
  (interactive)
  ;; TODO: check a custom var for num spaces to insert, hooks to control that var
  ;; depending on the mode
  (insert "    ")
)

;;(defun my-backspace ()
;;  "if tab, remove tab, if ws, remove ws"
;;  (interactive)
;;)

(global-set-key (kbd "TAB") 'my-insert-tab-char)
(global-set-key (kbd "<tab>") 'my-insert-tab-char)

(global-set-key (kbd "C-h .") nil)
(global-set-key (kbd "M-.") nil)
(global-set-key (kbd "<pause>") nil)

;; Navigate through buffers
(global-set-key (kbd "M-[") 'previous-buffer)
(global-set-key (kbd "M-]") 'next-buffer)
(global-set-key (kbd "<f12>") 'swiper)

;; enable line high-lighting only for some modes
(dolist (mode '(
                prog-mode-hook
		        text-mode-hook
                ))
  (add-hook mode (lambda() (hl-line-mode t)))
  )

(defconst stemacs-private-dir
  (if-let (stemacsdir (getenv-internal "EMACSDIR"))
      (expand-file-name (file-name-as-directory stemacsdir))
    (or (let ((xdgdir
               (expand-file-name "stemacs/"
                                 (or (getenv-internal "XDG_CONFIG_HOME")
                                     "~/.config"))))
          (if (file-directory-p xdgdir) xdgdir))
        "~/.stemacs.d/"))
  "Where your private configuration is placed.

Defaults to ~/.config/stemacs, ~/.stemacs.d or the value of the STEMACSDIR envvar;
whichever is found first. Must end in a slash.")

(add-to-list 'load-path (concat stemacs-private-dir "el-files"))

(require 'cl)
(require 'move-border)
(require 'tabularize)
(require 'misc-cmds)
(require 'text-manips)
(require 'interaction-log)

(setq ilog-print-lambdas t)

(define-key global-map [remap quit-window] 'delete-window-maybe-kill-buffer)

(defun srh/sp-forward-slurp-maybe ()
  (interactive)
  (when (featurep 'smartparens)
    (sp-forward-slurp-sexp))
  )

(defun srh/sp-backward-slurp-maybe ()
  (interactive)
  (when (featurep 'smartparens)
    (sp-backward-slurp-sexp))
  )

(defun srh/sp-forward-barf-maybe ()
  (interactive)
  (when (featurep 'smartparens)
    (sp-forward-barf-sexp))
  )

(defun srh/sp-backward-barf-maybe ()
  (interactive)
  (when (featurep 'smartparens)
    (sp-backward-barf-sexp))
  )

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
    (define-key map (kbd "C-s") #'swap-regions)
    (define-key map (kbd "C-q") #'kill-current-buffer)
    (define-key map (kbd "M-q") #'quoted-insert)
    (define-key map (kbd "C-<left>") 'srh/sp-backward-slurp-maybe)
    (define-key map (kbd "C-<right>") 'srh/sp-forward-slurp-maybe)
    (define-key map (kbd "C-S-<left>") 'srh/sp-backward-barf-maybe)
    (define-key map (kbd "C-S-<right>") 'srh/sp-forward-barf-maybe)
    map)
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

;; (global-set-key (kbd "M-/") 'hippie-expand)

;; You will most likely need to adjust this font size for your system!
(defvar srh/default-font-size 90)
(defvar srh/default-variable-font-size 110)

;;(ignore-errors (set-frame-font "Menlo-12")) ;; mono-spaced font ... ?
(set-face-attribute 'default nil :font "Fira Code Retina" :height srh/default-font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height srh/default-font-size)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height srh/default-variable-font-size :weight 'regular)

(defvar srh/tasksfile "~/Projects/learning-emacs/org-files/Tasks.org")
(defvar srh/bdaysfile "~/Projects/learning-emacs/org-files/Birthdays.org")
(defvar srh/emacsfile "~/Projects/learning-emacs/Emacs.org")

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa". "https://melpa.org/packages/")
                         ("org"  . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'recentf)
(add-to-list 'recentf-exclude "\\elpa")
(add-to-list 'recentf-exclude "\\melpa")
(add-to-list 'recentf-exclude "\\org")

(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-verbose t)

(defun backup-packages ()
  (interactive)
  (let* ((bakdir (format "%s~" package-user-dir)))
    (progn
      (condition-case nil
          (delete-directory bakdir t nil)
        (error (message "%s folder not found for deletion" bakdir)))
      (copy-directory package-user-dir bakdir t t t))
    )
  )

(use-package vlf
  :defer 1)

(use-package dash
  :config
  (global-dash-fontify-mode))
(with-eval-after-load 'info-look
  (dash-register-info-lookup))

(use-package no-littering
  :defer 1
  :config
  (setq
   auto-save-file-name-transforms
   `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
  (when (file-exists-p custom-file)
    (load custom-file)))

(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00")
  )
(add-hook 'auto-package-update-before-hook (lambda () (interactive) (backup-packages)))

(use-package autorevert
  :defer 1
  :ensure nil
  :delight auto-revert-mode
  :bind ("<f5>" . revert-buffer)
  :custom (auto-revert-verbose nil)
  :config (global-auto-revert-mode)
  )

(use-package sudo-edit
  :defer t)

(use-package dired
  :ensure nil ;; do not attempt to download/install
  :defer t
  :commands (dired dired-jump)
  :hook
  (dired-mode . dired-hide-details-mode)
  :bind
  (("C-x C-j" . dired-jump)
   :map dired-mode-map
   ([f5] . revert-buffer)
   )
  ([remap dired-mouse-find-file] . mouse-set-point)
  ([remap dired-mouse-find-file-other-frame] . mouse-set-point)
  ([remap dired-mouse-find-file-other-window] . mouse-set-point)
  :custom
  (require 'dired-x) ;; for -mark-extension and few others
  (dired-listing-switches "-AlthvX --group-directories-first")
  (dired-dwim-target 'dired-dwim-target-next)
  )

;; make dired do everything in a single buffer, and adds some shit called a magic-buffer ?!
(use-package dired-single
  :after dired
  :bind
  ([remap dired-find-file] . dired-single-buffer)
  ([remap dired-up-directory] . (lambda() (interactive) (dired-single-buffer "..")))
  ([remap dired-single-buffer-mouse] . mouse-set-point)
  )

;; cycle view sub-folder and sub-sub-folders in existing buffer
(use-package dired-subtree
  :after dired
  :config
  (bind-key "<tab>" #'dired-subtree-toggle dired-mode-map)
  (bind-key "<backtab>" #'dired-subtree-cycle dired-mode-map)
  )

;; fl = font-lock. Provided informative font changes based on file ext
(use-package diredfl
  :after dired
  :config
  (diredfl-global-mode))

(use-package dired-hide-dotfiles
  :after dired
  :commands (dired dired-jump)
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-hide-dotfiles-mode)
  )

;; NOTE: vterm depends on lots of extra OS installs and .shellrc stuff
;; In particular: make sure cmake points to /usr/bin/cmake, and not some
;; other path inserted by some other f*ing tool-chain (*cough* xilinx)
(use-package vterm
  :defer t
  :commands vterm
  :custom
  (vterm-max-scrollback 10000))

;; syntax highlighting of vimrc file
(use-package vimrc-mode)

(use-package hl-todo
  :defer t
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(("TODO"       warning bold)
          ("FIXME"      error bold)
          ("HACK"       font-lock-constant-face bold)
          ("REVIEW"     font-lock-keyword-face bold)
          ("NOTE"       success bold)
          ("DEPRECATED" font-lock-doc-face bold))))

(use-package all-the-icons
  :defer t)
(use-package all-the-icons-dired
  :after all-the-icons
  :hook
  ((dired-mode . all-the-icons-dired-mode)
   (dired-single-mode . all-the-icons-dired-mode)
   (wdired-change-to-wdired-mode . all-the-icons-dired-mode)
   )
  )

(defun ersatz-org-in-src-block-p (&optional inside)
  "Whether point is in a code source block.
When INSIDE is non-nil, don't consider we are within a src block
when point is at #+BEGIN_SRC or #+END_SRC."
  (and
   ;; In a src block
   (eq (car (org-element-at-point))
       'src-block)
   ;; Not at block delimiter, if requested
   (not (and inside
             (let ((case-fold-search t))
               (save-match-data
                 (save-excursion
                   (beginning-of-line)
                   (looking-at ".*#\\+\\(begin\\|end\\)_src"))))))))

;; (use-package aggressive-indent
;;   :defer 1
;;   :config
;;   (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
;;   (add-hook 'python-mode-hook #'aggressive-indent-mode)
;;   )

;; TODO: key bindings for smartparens - in a hydra or general-leader-key?
;; TODO: make smart-parens wrap current word if no space between parens and word - forwards and backwards
;; FIXME: this shit is BROKEN, wrapping and slurping can fuck up entire blocks of text, generally it's shit and sucks
(use-package smartparens
  :defer 1
  :delight
  :hook (prog-mode . smartparens-mode)
  :bind (
         ("C-'" . sp-backward-sexp)
         ("C-\\" . sp-forward-sexp)
         ("C-(" . sp-wrap-round)
         ("C-{" . sp-wrap-curly)
         ;; ("C-<" . (lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "<")))
         ;;("C-[" . sp-wrap-square)
         )
  :custom
  (sp-escape-quotes-after-insert nil)
  )
(with-eval-after-load 'smartparens
  (sp-local-pair '(emacs-lisp-mode) "'" "'" :actions nil)
  )

(use-package command-log-mode
  :defer 1
  :commands command-log-mode
  )

(use-package ivy
  :defer t
  :diminish
  :bind (
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-previous-i-search-kill)
         )
  :config
  (ivy-mode 1)
  )

;; NOTE: M-x all-the-icons-install-fonts to use below correctly
(use-package all-the-icons-ivy-rich
  :init (all-the-icons-ivy-rich-mode 1)
  )
(use-package ivy-rich
  :init (ivy-rich-mode 1)
  )

;; counsel + ivy for nice interactive mini-buffer completions
(use-package counsel
  :defer t
  :bind (("C-M-j" . 'counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  (ivy-initial-inputs-alist nil) ; don't start searches with ^
  :config
  (counsel-mode 1)
  )

(use-package ivy-prescient
  :after (counsel ivy)
  :custom
  (ivy-prescient-enable-filtering nil)
  :config
  ;; Uncomment the following line to have sorting remembered across sessions!
  (prescient-persist-mode 1)
  (ivy-prescient-mode 1))

(use-package solaire-mode
  :hook
  (after-make-frame-functions .  (lambda () (load-theme 'doom-tomorrow-night t)
                                   (solaire-mode-swap-faces-maybe)))
  (minibuffer-setup . solaire-mode-fix-minibuffer)
  :custom  
  (solaire-global-mode +1)
  )

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-height 30)
  (doom-modeline-icon (display-graphic-p))
  )

(use-package doom-themes
  :after solaire-mode
  :hook
  (server-after-make-frame . (lambda () (load-theme
					                'doom-tomorrow-night t)))
  )

(use-package beacon ;; This applies a beacon effect to the highlighted line
  :defer t
  :config
  (beacon-mode 1)
  )

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  )

(use-package rainbow-mode
  :defer 1
  :delight
  :hook ((prog-mode text-mode) . rainbow-mode)
  )

(use-package which-key
  :defer 1
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1.1)
  (setq which-key-separator " → ")
  (setq which-key-prefix-prefix nil)
  (set-face-attribute 'which-key-separator-face nil :weight 'bold :background "black" :foreground "white")
  (set-face-attribute 'which-key-key-face nil :background "black" :foreground "white")
  (set-face-attribute 'which-key-command-description-face nil :foreground "green" :background nil)
  (set-face-attribute 'which-key-group-description-face nil :foreground "green" :background nil)
  (setq which-key-unicode-correction 3)
  (setq which-key-add-column-padding 8)
  (setq which-key-sort-order nil)
  (setq which-key-max-display-columns 4)
  )

;; extended help page documentation adds source code, debugging links,
;; additional links to docu, details of where command referenced in other
;; elisp files, etc. good for assisting with own further customization
;; and with learning the emacs environment more effectively
(use-package helpful
  :after counsel
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  (
   ;; ([remap describe-function] . helpful-function)
   ([remap describe-command] . helpful-command)
   ;; ([remap describe-variable] . helpful-variable)
   ([remap describe-key] . helpful-key)
   (:map help-mode-map
         ("q" . (lambda() (interactive) (progn
                                     (helpful-kill-buffers)
                                     (delete-window))))
         )
   )
  )
(global-set-key (kbd "C-h q") #'helpful-kill-buffers)

;; TODO: if branches exist in undo tree at current node make u open the visualizer
;; TODO: make the visualizer show summary or preview or peek at content of branch
;; (message "%d" (undo-tree-num-branches))

(use-package undo-tree
  :bind
  ("<escape>" . undo-tree-visualizer-quit)
  :config
  (setq undo-tree-inhibit-kill-visualizer t)
  )
(use-package anzu
  :diminish
  :hook
  (after-init . global-anzu-mode)
  )

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-integration t)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-want-fine-undo t)
  (setq evil-undo-system 'undo-tree)
  :config
  (evil-mode 1)
  ;; much more vim like search interface when ex-mode / is used
  (evil-select-search-module 'evil-search-module 'evil-search)
  (evil-set-initial-state 'message-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)
  )

(use-package evil-anzu
  :after evil
  )

(use-package evil-collection
  :after (evil general)
  :config
  (evil-collection-init)
  (unbind-key "C-." 'evil-normal-state-map)
  )

;; TODO: make highlight apply across all visible buffers when used
;; highlight search persist and quick removal
(use-package evil-search-highlight-persist
  :defer t
  :after evil
  :config
  (global-evil-search-highlight-persist t)
  :custom
  (evil-search-highlight-persist-all-windows t)
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
  :bind ("M-/" . evilnc-comment-or-uncomment-lines)
  )

;; allows * search on visual/visual-line selections
(use-package evil-visualstar
  :defer 1
  :after evil
  :config
  (global-evil-visualstar-mode t)
  )

;; TODO: evil-snipe - use, customize, get acquainted

;; makes certain Vim like operator-state sequences operate on entire word
;; yw -> yiw, dw -> diw
;; this will only apply to the below specified commands; evil-yank/delete/change
;; source : https://stackoverflow.com/questions/37238920/key-mapping-in-evil-mode-emacs
(defun srh/evil-motion-range (orig-fun &rest args)
  (if (not (memq this-command '(evil-yank evil-delete)))
      (apply orig-fun args)
    (let* ((orig-keymap evil-operator-state-local-map)
           (evil-operator-state-local-map (copy-keymap orig-keymap)))
      (define-key evil-operator-state-local-map "w" "iw")
      (apply orig-fun args))))
(with-eval-after-load 'evil
  (advice-add 'evil-operator-range :around #'srh/evil-motion-range)
  )

(use-package general
  :defer t
  :config
  (general-evil-setup)
  (general-nmap "SPC l" (general-simulate-key "<pause>"))
  (general-nmap "SPC f" (general-simulate-key "<XF86WakeUp>"))
  (general-create-definer holy/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")
  (holy/leader-keys
    "/"  '(swiper :wk "Swiper")
    "f"  '(:ignore t :wk "Flycheck Commands")
    "l"  '(:ignore t :wk "LSP Commands")
    "d"  '(:ignore t :wk "Emacs Debug-Logging")
    "dd" '(srh/open-command-log :wk "open command log")
    "dD" '(clm/close-command-log-buffer :wk "hide command log")
    "g"  '(:ignore t :wk "Magit")
    "gg" '(magit-status :wk "magit-status")
    "o"  '(:ignore t :wk "Org")
    "os" '(org-schedule :wk "schedule")
    "ot" '(srh/eol-adj-org-time-stamp :wk "timestamp")
    "od" '(org-deadline :wk "deadline")
    "oo" '(org-agenda :wk "agenda commands")
    "o*" '(srh/toggle-org-hide-emph-markers :wk "hide/show emphasis")
    )
  (general-create-definer evil/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix ","
    :global-prefix "C-,")
  (evil/leader-keys
    :predicate '(derived-mode-p 'dired-mode)
    "/"   '(srh/toggle-dired-hide-details :wk "Dired-details")
    )
  (evil/leader-keys
    "s"   '(swap-regions :wk "Swap-regions")
    "c"   '(cycle-capitalizations :wk "Cycle-capitalizations")
    ;; don't display wk, my muscle memory is forever
    "SPC" '(srh/evil-nohl :wk t) 
    ":"   '(align-to-colon  :wk t)
    "<"   '(align-to-non-blocking-assign :wk t)
    "="   '(align-to-equals  :wk t)
    "("   '(align-to-open-paren  :wk t)
    "["   '(align-to-open-bracket  :wk t)
    ;; use wk for these
    "t"   '(hydra-tabularize-shortcuts/body :wk "Extra Tabularizations →")
    "S"   '(hydra-text-scale/body :wk "Scale Text →")
    "m"   '(hydra-evil-mc/body :wk "Multiple Cursors →")
    "e"   '(hydra-extended-chars/body :wk "Extended Characters →")
    "."   '(hydra-folder-shortcuts/body :wk "Folder Shortcuts →")
    )
  (general-create-definer dired-4-dummies/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'dired-mode-map
    :prefix "?"
    :global-prefix "C-?")
  (dired-4-dummies/leader-keys
    "" '(hydra-dired-options-4-dummies/body :wk t)
    )
  )

(use-package hydra
  :defer t
  ) ; transient key-bindings

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

;;(use-package projectile
;;  :diminish projectile-mode
;;  :config (projectile-mode)
;;  :custom (projectile-completion-system 'ivy)
;;  :bind-keymap
;;  ("C-c p" . projectile-command-map)
;;  :init
;;  (when (file-directory-p "~/Projects/Code")
;;    (setq projectile-project-search-path '("~/Projects/Code")))
;;  (setq projectile-switch-project-action #'projectile-dired))
;;
;;(use-package counsel-projectile
;;  :config (counsel-projectile-mode))

(defun magit-ls-files ()
  "List tracked files of current repository."
  (interactive)
  (if (derived-mode-p 'magit-mode)
      (magit-git-command "git ls-files")
    (message "Not in a Magit buffer."))
  )

(use-package magit
  :commands magit-status
  :custom
  (magit-display-current-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  )

(use-package libgit
  :after magit
  )

(defun srh/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1)
  (org-hide-block-startup t)
  )

(defun srh/org-setup-fonts ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  ;; change text style in org mode based on head-line level
  (dolist (face '(
                  (org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)
                  ))
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face))
    )
  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))

  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
  (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
  (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch)
  )

;; an outliner with a ton of other funcionality outside of writing outline docs
(use-package org
  :pin org
  :commands (org-capture org-agenda)
  :hook (org-mode . srh/org-mode-setup)
  :custom
  ;; (org-ellipsis " ▾")
  (org-ellipsis " ⤵")
  (org-hide-emphasis-markers t)
  (org-agenda-files '(srh/tasksfile
                      srh/bdaysfile))
  (org-agenda-start-with-log-mode t)
  (org-agenda-restore-windows-after-quit t)
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
                       (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(R)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANCELED(k@)")))
  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)  
  (org-habit-graph-column 60)
  (org-refile-targets
   '(("Archive.org":maxlevel . 1)
     ("Tasks.org"  :maxlevel . 1)))
  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)
  (org-tag-alist
   '((:startgroup)
     ;; Put mutually exclusive tags here
     (:endgroup)
     ("errand" . ?e)
     ("home" . ?h)
     ("work" . ?w)
     ("agenda" . ?a)
     ("planning" . ?p)
     ("publish" . ?P)
     ("batch" . ?b)
     ("note" . ?n)
     ("idea" . ?i)))
  ;; Configure custom agenda views
  (org-agenda-custom-commands
   '(("d" "Dashboard"
      ((agenda "" ((org-deadline-warning-days 7)))
       (todo "NEXT"
             ((org-agenda-overriding-header "Next Tasks")))
       (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))
     ("n" "Next Tasks"
      ((todo "NEXT"
             ((org-agenda-overriding-header "Next Tasks")))))
     ("W" "Work Tasks" tags-todo "+work-email")
     ;; Low-effort next actions
     ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
      ((org-agenda-overriding-header "Low Effort Tasks")
       (org-agenda-max-todos 20)
       (org-agenda-files org-agenda-files)))
     ("w" "Workflow Status"
      ((todo "WAIT"
             ((org-agenda-overriding-header "Waiting on External")
              (org-agenda-files org-agenda-files)))
       (todo "REVIEW"
             ((org-agenda-overriding-header "In Review")
              (org-agenda-files org-agenda-files)))
       (todo "PLAN"
             ((org-agenda-overriding-header "In Planning")
              (org-agenda-todo-list-sublevels nil)
              (org-agenda-files org-agenda-files)))
       (todo "BACKLOG"
             ((org-agenda-overriding-header "Project Backlog")
              (org-agenda-todo-list-sublevels nil)
              (org-agenda-files org-agenda-files)))
       (todo "READY"
             ((org-agenda-overriding-header "Ready for Work")
              (org-agenda-files org-agenda-files)))
       (todo "ACTIVE"
             ((org-agenda-overriding-header "Active Projects")
              (org-agenda-files org-agenda-files)))
       (todo "COMPLETED"
             ((org-agenda-overriding-header "Completed Projects")
              (org-agenda-files org-agenda-files)))
       (todo "CANCELED"
             ((org-agenda-overriding-header "Cancelled Projects")
              (org-agenda-files org-agenda-files)))))))
  (org-capture-templates
   `(("t" "Tasks / Projects")
     ("tt" "Task" entry (file+olp "~/Projects/Code/emacs-from-scratch/OrgFiles/Tasks.org" "Inbox")
      "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)
     ("j" "Journal Entries")
     ("jj" "Journal" entry
      (file+olp+datetree "~/Projects/Code/emacs-from-scratch/OrgFiles/Journal.org")
      "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
      ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
      :clock-in :clock-resume
      :empty-lines 1)
     ("jm" "Meeting" entry
      (file+olp+datetree "~/Projects/Code/emacs-from-scratch/OrgFiles/Journal.org")
      "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
      :clock-in :clock-resume
      :empty-lines 1)
     ("w" "Workflows")
     ("we" "Checking Email" entry (file+olp+datetree "~/Projects/Code/emacs-from-scratch/OrgFiles/Journal.org")
      "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)
     ("m" "Metrics Capture")
     ("mw" "Weight" table-line (file+headline "~/Projects/Code/emacs-from-scratch/OrgFiles/Metrics.org" "Weight")
      "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)))
  ;; end custom
  :config
  (message "org loaded")
  (define-key global-map (kbd "C-c j")
    (lambda () (interactive) (org-capture nil "jj")))
  (srh/org-setup-fonts)
  )

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●"))
  )

(defun srh/org-mode-visual-fill ()
  (setq visual-fill-column-width 200
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1)
  )

(use-package visual-fill-column
  :defer t
  :hook (org-mode . srh/org-mode-visual-fill)
  )

(defun srh/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode)
  )

(use-package pdf-tools
  :defer 1
  :config
  (pdf-tools-install)
  )

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . srh/lsp-mode-setup)
  :custom
  (lsp-keymap-prefix "<pause>")
  :config
  (lsp-enable-which-key-integration t)
  )

(use-package lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom)
  )

;; TODO: customize this
;; - open in new window adjacent to selected window
;; - or open in new window RHS
(use-package lsp-treemacs
  :after lsp-mode
  )

(use-package lsp-ivy
  :after (lsp-mode ivy)
  )

;; NOTE: company-manual-begin can be used to bring up completions
;;(use-package company
;;  :hook (
;;         (lsp-mode . company-mode)
;;         (prog-mode . company-mode)
;;         )
;;  :bind ((:map company-active-map
;;               ("<tab>" . company-complete-common-or-cycle)
;;               ("<f12>" . company-filter-candidates)
;;               )
;;         )
;;  :custom
;;  (company-minimum-prefix-length 1)
;;  (company-idle-delay 0.1)
;;  (company-require-match nil)
;;  (company-abort-on-unique-match nil)
;;  (company-transformers '(company-sort-by-backend-importance))
;;  (company-selection-wrap-around t)
;;  )
;;(global-company-mode)

(use-package markdown-mode
  :defer 1
  :hook ((markdown-mode . auto-fill-mode)
         ))

(use-package csv-mode
  :defer 1
  :config
  (setq csv-align-padding 2)
  (defun csv-align-visible ()
    "Align only visible entries in csv-mode"
    (interactive)
    (csv-align-fields nil
                      (window-start (selected-window))
                      (window-end (selected-window)))
    (message "Aligned visible fields only. Press C-c C-w to align again."))

  :bind (:map csv-mode-map ("C-c C-w" . 'csv-align-visible))
  :hook (csv-mode . csv-align-visible)
  )

(use-package json-mode)

(use-package python-mode
  :after lsp-mode
  :hook (python-mode . lsp-deferred)
  ;; :custom
  ;; NOTE: Set these if Python 3 is called "python3" on your system!
  ;; (python-shell-interpreter "python3")
  ;; (dap-python-executable "python3")
  )
  
(use-package pyvenv
  :after python-mode
  :config
  (pyvenv-mode 1)
  )

(use-package verilog-mode
  :after lsp-mode
  :config
  (require 'lsp-verilog)
  (setq verilog-indent-level             2
        verilog-indent-level-module      2
        verilog-indent-level-declaration 2
        verilog-indent-level-behavioral  2
        verilog-indent-level-directive   2
        verilog-case-indent              2
        verilog-auto-newline             t
        verilog-auto-indent-on-newline   t
        verilog-tab-always-indent        t
        verilog-auto-endcomments         t
        verilog-minimum-comment-distance 40
        verilog-indent-begin-after-if    t
        verilog-auto-lineup              'all
        )
  )

(custom-set-variables
 '(lsp-clients-svlangserver-launchConfiguration "/usr/bin/verilator -sv --lint-only -Wall -Wpedantic")
 '(lsp-clients-svlangserver-formatCommand "/usr/bin/verible-verilog-format")
 )

(add-hook 'verilog-mode-hook #'lsp-deferred)
(add-hook 'c-mode-hook #'lsp-deferred)

(use-package veri-kompass
  :after (verilog-mode lsp-verilog)
  )

(use-package term
  :defer t
  :config
  (setq explicit-shell-file-name "bash")
  )

(use-package sh-script
  :ensure nil
  :hook (after-save . executable-make-buffer-file-executable-if-script-p)
  )

(use-package swap-regions
  :commands swap-regions
  :config
  (set-face-attribute 'swap-regions-selection nil :background "black")
  )

;; TODO: replace with ivy-yasnippet
(use-package yasnippet
  :hook
  (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all)
  )
;; Add yasnippet support for all company backends
;; https://github.com/syl20bnr/spacemacs/pull/179
;;(defvar company-mode/enable-yas t
;;  "Enable yasnippet for all backends.")
;;(defun company-mode/backend-with-yas (backend)
;;  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
;;      backend
;;    (append (if (consp backend) backend (list backend))
;;            '(:with company-yasnippet))))
;;(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

(defun disable-flycheck-in-org-src-block ()
  (setq-local flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(add-hook 'org-src-mode-hook 'disable-flycheck-in-org-src-block)

(use-package flycheck
  :hook
  (prog-mode . flycheck-mode)
  :config
  (global-flycheck-mode)
  (setq-default flycheck-emacs-lisp-load-path 'inherit)
  (setq flycheck-global-modes '(not org-mode
                                    org-src-mode
                                    text-mode
                                    vterm-mode))
  (setq flycheck-highlighting-mode nil) ;; don't use that awful underline face
  (flycheck-define-error-level 'info :fringe-bitmap nil)
  (setq flycheck-error-list-minimum-level 'warning)
  (define-key flycheck-mode-map flycheck-keymap-prefix nil)
  (setq flycheck-keymap-prefix (kbd "<XF86WakeUp>"))
  (define-key flycheck-mode-map flycheck-keymap-prefix flycheck-command-map)
  )

;; TODO: see if you can get rid of this or replace with something better
;; NOTE: sudo yum install rpmdevtools, sudo apt-get install devscripts
(use-package flycheck-checkbashisms
  :after flycheck
  :config
  (flycheck-checkbashisms-setup)
  :custom
  (flycheck-checkbashisms-newline t)
  (flycheck-checkbashisms-posix t)
  )
(add-to-list 'display-buffer-alist
             `(,(rx bos "*Flycheck errors*" eos)
               (display-buffer-reuse-window
                display-buffer-in-side-window)
               (side            . bottom)
               (reusable-frames . visible)
               (window-height   . 0.25)))

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (dot . t)
     (makefile . t)
     (shell . t)
     (latex . t) ;; TODO: how to use this?
     (calc . t) ;; TODO: teach self how to use this
     ))
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
  (push '("conf-unix" . conf-unix) org-src-lang-modes)
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("dot" . "src dot :cmdline -Kdot -Tpng :file .default-dot-file.png")) ;; NOTE: graphviz
  (add-to-list 'org-structure-template-alist '("sh"  . "src shell :results silent"))
  (add-to-list 'org-structure-template-alist '("el"  . "src emacs-lisp :results silent"))
  (add-to-list 'org-structure-template-alist '("py"  . "src python :results output"))
  (setq org-confirm-babel-evaluate nil)
  (setq org-src-fontify-natively t)
  (setq org-edit-src-content-indentation 0)
  (setq org-src-tab-acts-natively t)
  (setq org-src-preserve-indentation t)
  ;; remap meta/shift related keys in org mode
  ;; I prefer to use meta/shift/arrow keys for switching and resizing windows
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

;; Automatically tangle our Emacs.org config file when we save it
(defun srh/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name srh/emacsfile))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))
(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'srh/org-babel-tangle-config)))

(defun srh/toggle-org-hide-emph-markers ()
  (interactive)
  (if (derived-mode-p 'org-mode)
      (if (equal org-hide-emphasis-markers t)
          (progn (setq org-hide-emphasis-markers nil) (org-mode))
        (progn (setq org-hide-emphasis-markers t) (org-mode)))
    (message "not in org-mode")))

(defun srh/append-space-if-eol ()
  (interactive)
  (if (equal (- (point-at-eol) 1) (point))
      (progn (end-of-line) (insert " ")))
  t)

(defun srh/eol-adj-org-time-stamp ()
  (interactive)
  (progn (srh/append-space-if-eol) (call-interactively 'org-time-stamp)))

(defun srh/open-command-log ()
  (interactive)
  (command-log-mode 1)
  (srh/add-to-list-multiple
   'clm/log-command-exceptions*
   '(
     left-char right-char evil-next-line evil-previous-line evil-insert
     evil-normal-state evil-append evil-end-of-line evil-forward-word-end
     evil-backward-word-begin delete-indentation evil-delete-char
     move-border-right move-border-left move-border-up move-border-down
     )
   )
  (clm/open-command-log-buffer)
  )

;; TODO: replace all highlighting with Modi's highlighting which uses highlight-global, volatile-highlights, & auto-highlight-symbol
(defun srh/evil-nohl ()
  (interactive)
  (if (equal evil-search-module 'evil-search)
      (evil-ex-nohighlight)
    (evil-search-highlight-persist-remove-all))
  )

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
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

;; remap home to `smarter-move-beginning-of-line'
(global-set-key [remap evil-beginning-of-line]
                'smarter-move-beginning-of-line)

(defun srh/stand-in-function (&optional msg ret)
  (interactive)
  (when msg (message "%s" msg))
  (let ((fret))
    (if ret
        (setq fret ret)
      (setq fret t))
    fret))

(defun srh/mc-toggle-cursors ()
  (interactive)
  (if (evil-mc-frozen-p)
      (evil-mc-resume-cursors)
    (evil-mc-pause-cursors)))

(defun srh/mc-select-matches ()
  (interactive)
  (evil-mc-execute-for-all-cursors
   (lambda (args)
     (interactive)
     (when (thing-at-point-looking-at (caar evil-mc-pattern))
       (if (alist-get :real args)
           (progn
             (goto-char (match-beginning 0))
             (evil-visual-char)
             (goto-char (- (match-end 0) 1)))
         (setq region (evil-mc-create-region
                       (match-beginning 0)
                       (match-end 0)
                       'char)))))))

(defun srh/toggle-cursor-at-pos ()
  (interactive)
  (unless (evil-mc-undo-cursor-at-pos (point))
    (evil-mc-make-cursor-here))
  )

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer-and-its-windows (delq (current-buffer) (buffer-list))))

(defvar tgl-all-mc-msg "Does nothing until you leave the mini-buffer")

;; ↓ from https://hungyi.net/posts/hydra-for-evil-mc/
(defhydra hydra-evil-mc (
                         :color pink
                         :hint nil
                         :pre (evil-mc-pause-cursors)
                         :post (progn
                                 (evil-mc-resume-cursors)
                                 (when evil-mc-pattern (srh/mc-select-matches))))
  "
_m_: make & next     _n_: move to next     _a_:  match all     _!_: freeze / unfreeze     _q_: quit
_M_: make & prev     _N_: move to prev     _R_: remove all     _f_: make / remove
Current pattern: %s(replace-regexp-in-string \"%\" \"%%\" (or (caar evil-mc-pattern) \"\"))  
"
  ("a" #'evil-mc-make-all-cursors)
  ("m" #'evil-mc-make-and-goto-next-match)
  ("M" #'evil-mc-make-and-goto-prev-match)
  ("n" (lambda() (interactive) (evil-mc-goto-cursor (evil-mc-find-next-cursor) t)))
  ("N" (lambda() (interactive) (evil-mc-goto-cursor (evil-mc-find-prev-cursor) t)))
  ("f" (lambda() (interactive) (srh/toggle-cursor-at-pos)))
  ("R" #'evil-mc-undo-all-cursors)
  ("!" (lambda() (interactive) (srh/stand-in-function tgl-all-mc-msg)))
  ("q" (message "") :color blue)
  ("<escape>" (message "") :color blue)
  )

;; TODO: setup machine depedendent hydra-defs
;; ie: for work-stations, folders with fpga-firmware, exanic-software, etc
(defhydra hydra-folder-shortcuts (
                                  :exit t
                                  :hint nil)
  "
^^^^Folders                               ^Files
^^^^-------------------------------       ^----------------^
_w_  Work            _p_  Projects        _t_  Tasks-File
_d_  Documents
_E_  Emacs
_P_  Playground

_._  %s(file-name-directory (or buffer-file-name load-file-name (concat \"file-name-directory for \" (format \"%s\" (current-buffer)) \" not found/unknown\")))
"
  ("w" (evil-window-vsplit nil "/data/shannam/Work"))
  ("E" (evil-window-vsplit nil  stemacs-private-dir))
  ("d" (evil-window-vsplit nil  "~/Documents"))
  ("p" (evil-window-vsplit nil  "~/Projects"))
  ("P" (evil-window-vsplit nil  "~/Playground"))
  ("t" (find-file-other-window srh/tasksfile))
  ("." (evil-window-vsplit nil "."))
  ("q" (message ""))
  ("<escape>" (message ""))
  ("RET" (message "") :color pink)
  )

(defhydra hydra-dired-options-4-dummies (
                                         :color pink
                                         :hint nil
                                         :pre (lambda() (interactive) (force-mode-line-update 'ALL))
                                         :post (lambda() (interactive) (force-mode-line-update nil))
                                         )
  "
^^^^applies to Marked xor thing-@-Point         ^to thing-@-Point ^^^                                ^to Buffer or All-in-Buffer^
^^^^------------------------------------        ^-----------------------------------------^^^        ^----------------------------^
_!_: shell cmd           _&_: async cmd         _m_: mark               _k_: hide line               _<kp-0>_: ↑ to parent dir
_D_: Delete              _G_: CHGRP             _u_: unmark             _w_: copy then hide          _<f5>_: [or g-r] revert buffer
_R_: Move                _O_: CHOWN             _c_: compress           _=_: diff                    _t_: invert marks
_K_: Hide Lines          _M_: CHMOD             _+_: MKDIR              _e_: new file                _h_: show/hide dotfiles
_F_: Open All            _Z_: TAR               _o_: open to other window [or S-RET] ^^              _(_: [or ,/] hide details
_L_: Load into Emacs     _T_: TOUCH                           
_C_: Copy                _H_: LN (hard)
^^                       _S_: LN (soft)          ^^ ^^                                             | _%_ regex : copy, rename, etc
_x_: delete flagged      _U_: Unmark All        _<tab>_: subtree expand/collapse ^^          OTHER | _*_ marks : all mark commands
                                                                                  ^^^^^^^^PREFIXES | _:_ crypt : encrypt/decrypt
Current switches: %s(format dired-listing-switches)
  "
  ("<kp-0>" (lambda() (interactive) (dired-single-buffer "..")))
  ("C" #'dired-do-copy)
  ("D" #'dired-do-delete)
  ("G" #'dired-do-chgrp)
  ("H" #'dired-do-hardlink)
  ("L" #'dired-do-load)
  ("M" #'dired-do-chmod)
  ("O" #'dired-do-chown)
  ("P" #'dired-do-print)
  ("R" #'dired-do-rename)
  ("S" #'dired-do-symlink)
  ("T" #'dired-do-touch)
  ("Z" #'dired-do-compress)
  ("c" #'dired-do-compress-to)
  ("!" #'dired-do-shell-command)
  ("&" #'dired-do-async-shell-command)
  ("m" #'dired-mark)
  ("h" #'dired-hide-dotfiles-mode)
  ("t" #'dired-toggle-marks)
  ("(" #'dired-hide-details-mode)
  ("o" #'dired-find-file-other-window)
  ("w" #'dired-copy-filename-as-kill)
  ("x" #'dired-do-flagged-delete)
  ("+" #'dired-create-directory)
  ("=" #'dired-diff)
  ("e" #'dired-create-empty-file)
  ("t" #'dired-toggle-marks)
  ("k" (lambda() (interactive) (dired-do-kill-lines 1)))
  ("K" #'dired-do-kill-lines)
  ("u" #'dired-unmark)
  ("U" #'dired-unmark-all-marks)
  ("F" (lambda() (interactive) (dired-do-find-marked-files)) :color blue)
  ("<tab>" #'dired-subtree-toggle)
  ("<f5>" #'revert-buffer)
  ("%" (call-interactively (srh/simulate-key-press "%")) :color blue)
  ("*" (call-interactively (srh/simulate-key-press "*")) :color blue)
  (":" (call-interactively (srh/simulate-key-press ":")) :color blue)
  ("q" (message "") :exit t)
  ("<escape>" (message "") :exit t)
  )

(defhydra hydra-tabularize-shortcuts (
                                      :exit t
                                      :hint nil)
  "
  _,_ before comma   _._ period          _>_ => 
  _;_ after comma    _'_ single-quote   
  "
  (","  (align-to-comma-before))
  (";"  (align-to-comma-after))
  ("."  (align-to-period))
  ("'"  (align-to-single-quote))
  (">"  (align-to-hash))
  )

(defhydra hydra-extended-chars (
                                :exit t
                                :hint nil)
  "
_<up>_     ↑      _8_  ★      _a_  α      _f_  ϕ      _k_  κ      _q_  θ      _w_  ω      _D_  Δ      _Q_  Θ
_<down>_   ↓      _6_  ◆      _b_  β      _g_  γ      _l_  λ      _r_  ρ      _x_  ξ      _G_  Γ      _S_  Σ
_<left>_   ←      _I_  ∞      _c_  χ      _h_  η      _m_  μ      _s_  σ      _y_  ψ      _L_  Λ      _W_  Ω
_<right>_  →      ^^          _d_  δ      _i_  ι      _n_  ν      _t_  τ      _z_  ζ      _N_  ∇      _X_  Ξ
^^                ^^          _e_  ε      _j_  φ      _p_  π      _u_  υ      ^^          _P_  Π      _Y_  Ψ

For quick-access use F9 + <key> when non in this hydra-head
"
  ("a" (insert "α"))
  ("b" (insert "β"))
  ("c" (insert "χ"))
  ("d" (insert "δ"))
  ("e" (insert "ε"))
  ("f" (insert "ϕ"))
  ("g" (insert "γ"))
  ("h" (insert "η"))
  ("i" (insert "ι"))
  ("j" (insert "φ"))
  ("k" (insert "κ"))
  ("l" (insert "λ"))
  ("m" (insert "μ"))
  ("n" (insert "ν")) 
  ("p" (insert "π"))
  ("q" (insert "θ"))
  ("r" (insert "ρ"))
  ("s" (insert "σ"))
  ("t" (insert "τ"))
  ("u" (insert "υ"))
  ("w" (insert "ω"))
  ("x" (insert "ξ"))
  ("y" (insert "ψ"))
  ("z" (insert "ζ"))
  ("D" (insert "Δ"))
  ("G" (insert "Γ"))
  ("L" (insert "Λ"))
  ("N" (insert "∇"))
  ("P" (insert "Π"))
  ("Q" (insert "Θ"))
  ("S" (insert "Σ"))
  ("W" (insert "Ω"))
  ("X" (insert "Ξ"))
  ("Y" (insert "Ψ"))
  ("<down>" (insert "↓"))
  ("<left>" (insert "←"))
  ("<right>" (insert "→"))
  ("<up>" (insert "↑"))
  ("8" (insert "★"))
  ("6" (insert "◆"))
  ("I" (insert "∞"))
  )

(defhydra hydra-text-scale (
                            :color pink
                            :hint nil)
  "
text size
_+_ increase    
_-_ decrease
"
  ("+" text-scale-increase)
  ("-" text-scale-decrease)
  ("q" (message "") :exit t)
  ("<escape>" (message "") :exit t)
  )

(defun srh/toggle-dired-hide-details ()
  (interactive)
  (if (derived-mode-p 'dired-mode)
      (dired-hide-details-mode 'toggle)
    (message "not in dired-mode")))

(defun srh/display-interaction-log ()
  (interactive)
  (progn
    (interaction-log-mode 'toggle)
    (display-buffer-in-side-window (get-buffer "*Emacs Log*")
                                   '((side . right)))))

(defun srh/delete-window-maybe-kill-buffer-maybe-delete-frame ()
  (interactive)
  (if (eq (count-windows) 1)
      (evil-ex-call-command nil "quit" nil)
    (delete-window-maybe-kill-buffer))
  )

(with-eval-after-load 'evil
  (setq evil-emacs-state-cursor '("#81a2be" box))
  (setq evil-normal-state-cursor '("#81a2be" box))
  (setq evil-visual-state-cursor '("orange" box))
  (setq evil-insert-state-cursor '("green" bar))
  (setq evil-replace-state-cursor '("red" box))
  (setq evil-operator-state-cursor '("red" hollow))
  (evil-define-key '(normal) 'global  (kbd "M-.") #'helpful-at-point)
  (evil-define-key '(normal visual insert) 'global  (kbd "M-DEL") 'sp-unwrap-sexp)
  (evil-define-key '(normal visual) 'global (kbd "+") 'evil-numbers/inc-at-pt-incremental)
  (evil-define-key '(normal visual) 'global (kbd "-") 'evil-numbers/dec-at-pt-incremental)
  (evil-define-key '(normal visual) 'global (kbd "C-+") 'evil-numbers/inc-at-pt)
  (evil-define-key '(normal visual) 'global (kbd "C--") 'evil-numbers/dec-at-pt)
  (evil-define-key '(normal visual) 'global (kbd "R") 'evil-mc-undo-all-cursors)
  (evil-define-key '(normal visual) 'global (kbd "!") 'srh/mc-toggle-cursors)
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
  (evil-ex-define-cmd "q" 'srh/delete-window-maybe-kill-buffer-maybe-delete-frame)
  (evil-ex-define-cmd "aq" 'kill-other-buffers)
  ;; Need to type out :quit to close emacs
  (evil-ex-define-cmd "quit" 'evil-quit)
  (global-undo-tree-mode)
  (turn-on-undo-tree-mode)
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
(with-eval-after-load 'flycheck
  (modify-syntax-entry ?- "w" flycheck-error-list-mode-syntax-table)
  (modify-syntax-entry ?_ "w" flycheck-error-list-mode-syntax-table)
  )

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

(defun common-init-calls ()
  (set-face-attribute 'default nil
                      :font  "Fira Code Retina"
                      :weight 'normal
                      :height srh/default-font-size)
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

;; TODO: setup emacs daemon and emacs client → reverse sshfs ideapad to work to use ideapad emacs server? Or some other way

(defun client-frame-init ()
  (common-init-calls)
  (message "emacs client post-frame init complete!")
  )

(defun standalone-frame-init ()
  (common-init-calls)
  (load-theme 'doom-tomorrow-night t)
  (message "emacs standalone post-frame init complete!")
  )

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (select-frame frame)
                (client-frame-init)))
  (standalone-frame-init)
  )

;; TODO: add-hook functionality to swap or duplicate regions

(global-set-key (kbd "M-p") #'me/swap-up)
(global-set-key (kbd "M-n") #'me/swap-down)
(global-set-key (kbd "M-P") #'me/duplicate-backward)
(global-set-key (kbd "M-N") #'me/duplicate-forward)

(defun me/duplicate-line (&optional stay)
  "Duplicate current line.
With optional argument STAY true, leave point where it was."
  (save-excursion
    (move-end-of-line nil)
    (save-excursion
      (insert (buffer-substring (point-at-bol) (point-at-eol))))
    (newline))
  (unless stay
    (let ((column (current-column)))
      (forward-line)
      (forward-char column))))

(defun me/duplicate-backward ()
  "Duplicate current line upward or region backward.
If region was active, keep it so that the command can be repeated."
  (interactive)
  (if (region-active-p)
      (let (deactivate-mark)
        (save-excursion
          (insert (buffer-substring (region-beginning) (region-end)))))
    (me/duplicate-line t)))

(defun me/duplicate-forward ()
  "Duplicate current line downward or region forward.
If region was active, keep it so that the command can be repeated."
  (interactive)
  (if (region-active-p)
      (let (deactivate-mark (point (point)))
        (insert (buffer-substring (region-beginning) (region-end)))
        (push-mark point))
    (me/duplicate-line)))

(defun me/swap-down ()
  "Move down the line under point."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(defun me/swap-up ()
  "Move up the line under point."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(provide 'init)

;;; init.el ends here
