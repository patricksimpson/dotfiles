;;; markdowon
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;;; HTML/CSS
(use-package web-mode
  :ensure t
  :config (progn
           (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
           (add-to-list 'auto-mode-alist '("\\.ejs?\\'" . web-mode))
           (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))))

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-scss-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(add-hook 'web-mode-hook  'my-web-mode-hook)

;;; JSON
(use-package json-mode
  :ensure t)

(use-package company
  :ensure t)

(add-hook 'after-init-hook 'global-company-mode)

(use-package highlight-indentation
  :ensure t)

(use-package dired-narrow
  :ensure t
  :bind (:map dired-mode-map
              ("/" . dired-narrow-fuzzy)))

(use-package dired-subtree
  :ensure t
  :bind (:map dired-mode-map
              ("i" . dired-subtree-toggle)))

(use-package flycheck
  :ensure t
  :defer 1
  :bind ("C-c '" . flycheck-mode)
  :config (progn
            (advice-add 'flycheck-eslint-config-exists-p :override (lambda() t))))

(use-package tide
  :ensure t
  :after flycheck
  :defer 1
  :config (progn
            (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)))

(use-package yaml-mode
  :ensure t)

(use-package dockerfile-mode
  :ensure t)


(add-hook 'js-mode-hook #'flycheck-mode)
(add-hook 'ruby-mode-hook #'flycheck-mode)
(add-hook 'css-mode-hook #'rainbow-delimiters-mode)
(add-hook 'scss-mode-hook #'rainbow-delimiters-mode)
(add-hook 'less-mode-hook #'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(setq ag-highlight-search t)

(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
;;; ide.el ends here
