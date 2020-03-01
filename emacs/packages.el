(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package ivy
  :ensure t
  :config (progn
            (ivy-mode 1)
            (setq ivy-use-virtual-buffers t)
            (setq ivy-count-format "(%d/%d) ")))


;;; Ivy mini buffer bindings
(define-key ivy-minibuffer-map (kbd "C-n") #'ivy-next-line)
(define-key ivy-minibuffer-map (kbd "C-p") #'ivy-previous-line)
;;; Ivy mini buffer vim bindings
(define-key ivy-minibuffer-map (kbd "C-h") (kbd "DEL"))
(define-key ivy-minibuffer-map (kbd "C-k") #'ivy-previous-line)
(define-key ivy-minibuffer-map (kbd "C-j") #'ivy-next-line)
(define-key ivy-minibuffer-map (kbd "C-l") #'ivy-alt-done)

(setq search-default-mode #'char-fold-to-regexp)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
(setq counsel-mode t)

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

(use-package ag
  :ensure t)

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
