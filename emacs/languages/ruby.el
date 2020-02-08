(use-package inf-ruby
  :ensure t)

(use-package rspec-mode
  :ensure t
  :defer 1
  :mode ("*spec.rb" . rspec-mode))

(defadvice rspec-compile (around rspec-compile-around)
  (let ((shell-file-name "/bin/bash"))
    ad-do-it))

(use-package rubocopfmt
  :ensure t)
    ;; :init (progn
    ;; (add-hook 'ruby-mode-hook #'rubocopfmt-mode)))

(use-package projectile-rails
  :ensure t)

(use-package robe
  :ensure t
  :config (progn
            (add-hook 'ruby-mode-hook 'robe-mode)))

(projectile-rails-global-mode)
(ad-activate 'rspec-compile)
(eval-after-load 'company
  '(push 'company-robe company-backends))

(defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
  (rvm-activate-corresponding-ruby))

;;; ruby.el ends here
