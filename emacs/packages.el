(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package ivy
  :ensure t
  :config (progn
            (ivy-mode 1)
            (setq ivy-use-virtual-buffers t)
            (setq ivy-count-format "(%d/%d) ")))

(use-package magit
  :ensure t)

(use-package projectile
  :ensure t
  :bind (("C-c b" . projectile-switch-project)
         ("C-c p" . projectile-find-file))
  :config (progn
            (projectile-mode)
            (setq projectile-enable-caching nil)
            (setq projectile-switch-project-action 'projectile-find-file)
            (setq projectile-completion-system 'ivy)))

(use-package counsel-projectile
  :ensure t)
