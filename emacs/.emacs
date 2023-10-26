(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook #'(lambda() 
			       (setq gc-cons-threshold 800000)))
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Bootstrap 'use-package'
(eval-after-load 'gnutls
  '(add-to-list 'gnutls-trustfiles "/etc/ssl/cert.pem"))
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'bind-key)
(setq use-package-always-ensure t)

(load "~/dotfiles/emacs/settings.el")
(load "~/dotfiles/emacs/functions.el")
(load "~/dotfiles/emacs/themes.el")
(load "~/dotfiles/emacs/packages.el")
(load "~/dotfiles/emacs/evil.el")
(load "~/dotfiles/emacs/ide.el")
(load "~/dotfiles/emacs/languages/ruby.el")
(load "~/dotfiles/emacs/languages/javascript.el")
(load "~/dotfiles/emacs/ivy.el")
(load "~/dotfiles/emacs/hydras.el")
(load "~/dotfiles/emacs/org.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(flycheck-yamllint pbcopy lsp-mode disable-mouse mood-line nerd-icons-ivy-rich nerd-icons-completion doom-modeline-now-playing mmm-mode rainbow-mode neotree wgrep-ag import-js graphql-mode prettier-js rainbow-delimiters evil-surround wrap-region rjsx-mode org-present csv-mode ivy-rich evil-org multi-term evil-matchit ruby-electric company-robe exec-path-from-shell hydra xref-js2 js2-refactor js2-mode json-mode web-mode markdown-mode counsel-projectile projectile evil-leader evil-magit magit ivy doom-themes doom-modeline use-package evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
