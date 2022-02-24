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

(use-package all-the-icons-completion
  :ensure t)

(all-the-icons-completion-mode)
;run once and forget it.
;; (all-the-icons-install-fonts )
