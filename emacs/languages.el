;Markdowon
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;HTML/CSS
(use-package web-mode
  :ensure t
  :config (progn
           (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
           (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))))

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(add-hook 'web-mode-hook  'my-web-mode-hook)

;JSON
(use-package json-mode
  :ensure t)
