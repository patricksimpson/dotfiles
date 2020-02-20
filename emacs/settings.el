(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode)

(fset 'yes-or-no-p 'y-or-n-p)

(setq visible-bell nil)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq inhibit-splash-screen t)
(setq confirm-kill-emacs 'yes-or-no-p)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default css-indent-offset 2)
(setq-default typescript-indent-level 2)
(setq-default visual-fill-column-width 160)

(set-default 'truncate-lines t)

(add-hook 'visual-line-mode-hook 'visual-fill-column-mode)
(add-hook 'find-file-hooks 'assume-new-is-modified)

(defun assume-new-is-modified ()
  (when (not (file-exists-p (buffer-file-name)))
    (set-buffer-modified-p t)))

(global-set-key (kbd "C-SPC") nil)

(global-display-line-numbers-mode)
(setq-default text-scale-mode-step 1.1)

(set-face-attribute 'default nil :font "Monaco-12")
(set-frame-font "Monaco-12" nil t)

;;; OSX super bindings
(global-set-key (kbd "s-b") 'projectile-switch-project)
(global-set-key (kbd "s-=") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)
(global-set-key (kbd "s-0") 'text-scale-adjust)
(global-set-key (kbd "s-p") 'projectile-find-file)
(define-key global-map (kbd "s-t") nil)
