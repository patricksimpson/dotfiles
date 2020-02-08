(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package evil
	     :ensure t
	     :config (progn
		(evil-mode 1)))

(use-package doom-modeline
      :ensure t
      :defer t
      :init (add-hook 'after-init-hook #'doom-modeline-mode))

(use-package doom-themes
  :ensure t
  :init (load-theme 'doom-tomorrow-night t))
