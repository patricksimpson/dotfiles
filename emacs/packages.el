(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package magit
  :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :config (progn
            (rainbow-delimiters-mode))

(use-package import-js
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
:ensure t)

(use-package add-node-modules-path
  :ensure t)

(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))
(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

(use-package org
  :ensure t
  :defer 2
  :mode (
         ("\\.txt\\'" . org-mode)))

(use-package multi-term
  :ensure t
  :config (progn
    (setq multi-term-program "/bin/zsh")
    ;; (setq multi-term-program-switches "--login")
    (define-key global-map (kbd "C-p") 'term-paste)))

(use-package evil-org
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package csv-mode
  :ensure t)

(use-package org-present
  :ensure t)

(use-package neotree
  :ensure t)

(with-eval-after-load 'neotree
  (add-hook 'neotree-mode-hook 
    (lambda () (with-current-buffer " *NeoTree*"
      (setq-local linum-mode nil)))))

(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

(use-package nerd-icons
  :ensure t
  ;; :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  )
(use-package pbcopy
  :ensure t)

(turn-on-pbcopy)
