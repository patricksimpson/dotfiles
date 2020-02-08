(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook #'(lambda() 
			       (setq gc-cons-threshold 800000)))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)


(load "~/.emacs.d/settings.el")
(load "~/.emacs.d/functions.el")
(load "~/.emacs.d/themes.el")
(load "~/.emacs.d/packages.el")
(load "~/.emacs.d/evil.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (counsel-projectile projectile evil-leader evil-magit magit ivy doom-themes doom-modeline use-package evil))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
