(use-package evil-magit
  :ensure t)

(use-package evil-leader
  :ensure t
  :config (progn
            (global-evil-leader-mode)
            (evil-leader/set-leader ",")
            (define-key evil-normal-state-map "\C-p" 'projectile-find-file)
            (define-key evil-normal-state-map (kbd "C-=") 'counsel-switch-buffer-other-window)
            (define-key evil-normal-state-map (kbd "C-b") 'projectile-switch-project)
            (define-key evil-normal-state-map (kbd "C-m") 'magit)
            (evil-leader/set-key "v" 'simpson-vertical-split
                                 "e" 'find-file
                                 "=" 'switch-to-buffer
                                 "f" 'counsel-projectile-ag
                                 "v" 'evil-window-vnew
                                 "x" 'evil-window-new
                                 "r" 'redraw-display
                                 "l" 'display-line-numbers-mode)
            (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
            (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
            (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
            (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
            (define-key evil-normal-state-map (kbd "gc")  'simpson-comment)
                                                
            (evil-ex-define-cmd "W" 'save-buffer)))

;evil mode is turned on after evil-leader for initial buffer support.
(use-package evil
  :ensure t
  :config (progn
            (evil-mode 1)))

(use-package evil-matchit
  :ensure t
  :config (progn
    (global-evil-matchit-mode 1)))
;;; evil.el ends here
