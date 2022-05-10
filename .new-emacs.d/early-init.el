(setq gc-cons-threshold most-positive-fixnum)

(setq native-comp-deferred-compilation nil)

;; disable package.el in favor of straight.el
(setq package-enable-at-startup nil)

(setq-default inhibit-redisplay t
              inhibit-message t)
(add-hook 'window-setup-hook
          (lambda ()
            (setq-default inhibit-redisplay nil
                          inhibit-message nil)
            (redisplay)))

(set-language-environment "UTF-8")
(setq default-input-method nil)
