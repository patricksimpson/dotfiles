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
