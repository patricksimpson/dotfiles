(use-package prettier-js
  :ensure t)

(require 'prettier-js)
(require 'rainbow-delimiters)

(use-package eslint-rc
  :ensure t)

(use-package js2-mode
  :ensure t
  :after flycheck
  :defer 1
  :config (progn
            (setq js-indent-level 2)
            (setq js2-basic-offset 2)
            (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))))

(use-package rjsx-mode
  :ensure t
  :after flycheck
  :defer 1
  :config (progn
            (setq js-indent-level 2)
            (setq js2-basic-offset 2)
            (add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))
            (add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode))))
  
(use-package js2-refactor
  :ensure t)

(use-package xref-js2
  :ensure t)

(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-r")
(define-key js2-mode-map (kbd "C-k") #'js2r-kill)

;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
;; unbind it.
(define-key js-mode-map (kbd "M-.") nil)

(add-hook 'js2-mode-hook (lambda ()
                           (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))

(add-hook 'js2-mode-hook
  (function (lambda ()
              (setq evil-shift-width js-indent-level))))

(add-hook 'rjsx-mode-hook
  (function (lambda ()
              (setq evil-shift-width js-indent-level))))
(add-hook 'rjsx-mode-hook 'eslint-rc-mode)
(add-hook 'rjsx-mode-hook #'eslint-fix-auto-mode)
(add-hook 'rjsx-mode-hook 'prettier-js-mode)
(add-hook 'js2-mode-hook 'prettier-js-mode)
(add-hook 'rjsx-mode-hook 'rainbow-delimiters-mode-enable)
(add-hook 'js2-mode-hook 'rainbow-delimiters-mode-enable)
