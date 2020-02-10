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
            (setq projectile-indexing-method 'hybrid)
            (setq projectile-enable-caching nil)
            (setq projectile-switch-project-action 'projectile-find-file)
            (setq projectile-project-search-path '("~/projects"))
            (setq projectile-completion-system 'ivy)
            (add-to-list 'projectile-globally-ignored-directories "*node_modules")
            (add-to-list 'projectile-globally-ignored-directories "*bower_components")))

(use-package counsel-projectile
  :ensure t)

(use-package exec-path-from-shell
  :ensure t
  :config (progn
            (when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))))
