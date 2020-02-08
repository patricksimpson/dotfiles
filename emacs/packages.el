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

(use-package all-the-icons
  :defer 1
  :ensure t)

;run once and forget it.
;(all-the-icons-install-fonts )

(use-package ivy
  :ensure t
  :config (progn
            (ivy-mode 1)
            (setq ivy-use-virtual-buffers t)
            (setq ivy-count-format "(%d/%d) ")))

(use-package magit
  :ensure t)

(use-package evil-magit
    :ensure t)
