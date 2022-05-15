(setq gc-cons-threshold most-positive-fixnum
      inhibit-startup-message t
      initial-scratch-message nil
      sentence-end-double-space nil
      ring-bell-function 'ignore
      use-dialog-box nil
      case-fold-search nil
      compilation-scroll-output t
      load-prefer-newer t
      help-window-select t
      package-enable-at-startup nil
      make-backup-files nil
      auto-save-default nil
      create-lockfiles nil
      custom-file null-device
      native-comp-deferred-compilation nil)

(setq-default inhibit-redisplay t
              inhibit-message t)
(add-hook 'window-setup-hook
          (lambda ()
            (setq-default inhibit-redisplay nil
                          inhibit-message nil)
            (redisplay)))

(set-language-environment "UTF-8")
(setq default-input-method nil)
