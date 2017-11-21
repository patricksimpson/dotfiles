(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook #'(lambda ()
                               ;; restore after startup
                               (setq gc-cons-threshold 800000)))
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
'("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
; (custom-set-variables
;  ;; custom-set-variables was added by Custom.
;  ;; If you edit it by hand, you could mess it up, so be careful.
;  ;; Your init file should contain only one such instance.
;  ;; If there is more than one, they won't work right.
;  '(ansi-color-names-vector
;    ["#151718" "#Cd3f45" "#9fca56" "#e6cd69" "#55b5db" "#a074c4" "#55b5db" "#d6d6d6"])
;  '(safe-local-variable-values (quote ((css-indent-offset . 4) (evil-shift-width . 4)))))

;http://emacs.stackexchange.com/questions/7372/stray-trailing-4m-before-prompt-with-zsh-in-m-x-ansi-term
;also good to know to 'send-eof' in helm

;http://stackoverflow.com/a/6697964/2344737
;(setq split-height-threshold nil)
;(setq split-width-threshold 0)

;prevent new frame
;(defun switch-to-buffer-other-frame ())

(load "~/.emacs.d/settings.el")
(load "~/.emacs.d/keybinds.el")

;https://www.gnu.org/software/emacs/manual/html_node/emacs/Directory-Variables.html
;set local variable per project :point_up:
(put 'narrow-to-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-ag-base-command "ag --nocolor --nogroup")
 '(package-selected-packages
   (quote
    (ivy tide php-mode sauron web-mode handlebars-mode all-the-icons-dired doom-themes all-the-icons neotree visual-fill-column evil-magit editorconfig yasnippet which-key vimish-fold use-package reveal-in-osx-finder relative-line-numbers osx-trash multi-term markdown-mode magit key-chord js2-mode helm-projectile helm-flyspell helm-ag flycheck fill-column-indicator exec-path-from-shell evil-matchit evil-leader escreen emmet-mode dired-subtree dired-narrow diff-hl deft base16-theme avy auto-complete alert))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
