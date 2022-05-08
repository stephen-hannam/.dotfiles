(add-to-list 'load-path (expand-file-name "modules/" user-emacs-directory))

(load-theme 'tango-dark t)
(set-face-attribute 'default nil :height 100)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; straight.el - bootstrap straight package manager if need be

(defvar bootstrap-version)
(let ((bootstrap-file
      (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
        "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
        'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
