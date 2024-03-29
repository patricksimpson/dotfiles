(use-package doom-themes
  :ensure t
  :init (load-theme 'doom-tomorrow-night t))

(use-package all-the-icons
   :defer 1
   :ensure t)

(use-package mood-line
   :ensure t
   ;; Enable mood-line
   :config
   (mood-line-mode))

(setq mood-line-glyph-alist mood-line-glyphs-unicode)

(use-package all-the-icons-completion
   :ensure t)

(all-the-icons-completion-mode)
;run once and forget it.
;; (all-the-icons-install-fonts )
;; For doom-mode-line
;; (nerd-icons-install-fonts ) 
