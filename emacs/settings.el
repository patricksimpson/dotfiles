(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode)

(fset 'yes-or-no-p 'y-or-n-p)

(setq visible-bell nil)
(setq temporary-file-directory "~/temp/emacs")
(setq backup-by-copying t)
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;disable backup
(setq backup-inhibited t)
;disable auto save
(setq auto-save-default nil)

(setq inhibit-splash-screen t)
(setq confirm-kill-emacs 'yes-or-no-p)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)
(setq-default css-indent-offset 2)
(setq-default web-mode-markup-indent-offset 2)
(setq-default typescript-indent-level 2)
(setq-default visual-fill-column-width 160)

(set-default 'truncate-lines t)

(add-hook 'visual-line-mode-hook 'visual-fill-column-mode)
(add-hook 'find-file-hooks 'assume-new-is-modified)

(defun assume-new-is-modified ()
  (when (not (file-exists-p (buffer-file-name)))
    (set-buffer-modified-p t)))

(global-set-key (kbd "C-SPC") nil)
(global-set-key (kbd "C-c k") 'simpson-copy-current-file-path)

(global-display-line-numbers-mode 1)
(setq display-line-numbers 'relative)
(setq display-line-numbers-type 'relative)
(setq-default text-scale-mode-step 1.1)

(set-face-attribute 'default nil :font "Monaco-14")
(set-frame-font "Monaco-14" nil t)

(setq scroll-preserve-screen-position t)
(set-fontset-font t 'symbol "Apple Color Emoji")
(set-fontset-font t 'symbol "Noto Color Emoji" nil 'append)
(set-fontset-font t 'symbol "Segoe UI Emoji" nil 'append)
(set-fontset-font t 'symbol "Symbola" nil 'append)

;; (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji")
;;                   frame 'prepend)

(when (member "Noto Color Emoji" (font-family-list))
  (set-fontset-font
    t 'symbol (font-spec :family "Noto Color Emoji") nil 'prepend))

(flyspell-mode)

;;; stop making backup files!!!
(setq make-backup-files nil)

;;; OSX super bindings
(global-set-key (kbd "s-b") 'projectile-switch-project)
(global-set-key (kbd "s-=") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)
(global-set-key (kbd "s-0") 'text-scale-adjust)
(global-set-key (kbd "s-p") 'projectile-find-file)
(define-key global-map (kbd "s-t") nil)

(setq-default dired-details-hidden-string "---")
(add-hook 'dired-mode-hook 'dired-hide-details-mode)
(setq dired-dwim-target t)
(setq history-length 25)
(savehist-mode 1)
(recentf-mode 1)
(save-place-mode 1)
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

;; (define-globalized-minor-mode my-global-rainbow-mode rainbow-mode
;;   (lambda () (rainbow-mode 1)))

;; (my-global-rainbow-mode 1)
