(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

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

(use-package evil-leader
  :ensure t
  :config (progn
            (global-evil-leader-mode)
            (evil-leader/set-leader ",")
            (evil-leader/set-key "v" 'simpson-vertical-split)
            (define-key evil-normal-state-map "\C-p" 'projectile-find-file)
            (define-key evil-normal-state-map (kbd "C-=") 'counsel-switch-buffer-other-window)
            (define-key evil-normal-state-map (kbd "C-b") 'projectile-switch-project)
            (define-key evil-normal-state-map (kbd "C-m") 'magit)
            (evil-leader/set-key "f" 'counsel-projectile-ag)
            (evil-leader/set-key "v" 'evil-window-vnew)
            (evil-leader/set-key "x" 'evil-window-new)
            (evil-leader/set-key "r" 'redraw-display)
            (evil-leader/set-key "l" 'display-line-numbers-mode)
            (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
            (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
            (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
            (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
            (evil-ex-define-cmd "W" 'save-buffer)))

;evil mode is turned on after evil-leader for initial buffer support.
(use-package evil
  :ensure t
  :config (progn
            (evil-mode 1)))

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

(defun simpson-vertical-split()
  (interactive)
  (split-window-right)
  (evil-window-right 1))

(defun simpson-reload-emacs-config()
 "Reload emacs config."
 (interactive)
 (load-file "~/dotfiles/emacs/.emacs"))
