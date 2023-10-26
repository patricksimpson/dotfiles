(use-package evil-leader
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  :config (progn
            (global-evil-leader-mode)
            (evil-leader/set-leader ",")
            (define-key evil-normal-state-map "\C-p" 'counsel-projectile-find-file)
            (define-key evil-normal-state-map (kbd "C-=") 'counsel-switch-buffer)
            (define-key evil-normal-state-map (kbd "C-b") 'counsel-projectile-switch-project)
            (define-key evil-normal-state-map (kbd "C-m") 'magit)
            (evil-leader/set-key "v" 'simpson-vertical-split
                                 "e" 'find-file-at-point
                                 "a" 'counsel-find-file
                                 "=" 'counsel-switch-to-buffer
                                 "f" 'counsel-projectile-ag
                                 "g" 'project-find-regexp
                                 "v" 'evil-window-vsplit
                                 "h" 'evil-window-split
                                 "d" 'simpson-copy-current-file-path
                                 "t" 'simpson-edit-emacs-settings
                                 "x" 'evil-window-new
                                 "c" 'save-some-buffers
                                 "n" 'neotree-project-dir
                                 "r" 'redraw-display
                                 "l" 'display-line-numbers-mode)
            (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
            (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
            (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
            (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
            (define-key evil-normal-state-map (kbd "gc")  'simpson-comment)

            (define-key evil-normal-state-map (kbd "C-d") 'evil-scroll-page-down)
            (define-key evil-visual-state-map (kbd "C-d") 'evil-scroll-page-down)
            (define-key evil-insert-state-map (kbd "C-d")
              (lambda ()
                (interactive)
                (evil-delete (point-at-bol) (point))))
            (define-key evil-normal-state-map (kbd "C-S-d") 'evil-scroll-page-up)
            (define-key evil-visual-state-map (kbd "C-S-d") 'evil-scroll-page-up)
            (define-key evil-insert-state-map (kbd "C-S-d")
              (lambda ()
                (interactive)
                (evil-delete (point-at-bol) (point))))
            (setq evil-shift-width 2)
            (evil-ex-define-cmd "W" 'save-buffer)))
			
			(global-unset-key (kbd "C-z"))
			(global-set-key (kbd "C-z")   'undo-fu-only-undo)
			(global-set-key (kbd "C-r") 'undo-fu-only-redo)
			(global-set-key (kbd "C-u") 'undo-fu-only-redo)
;; (setq evil-shift-width 2)
;; ;evil mode is turned on after evil-leader for initial buffer support.
(use-package evil
   :ensure t
   :init
   (setq evil-want-keybinding nil)
   (setq evil-undo-system 'undo-fu)
   :config (progn
             (evil-mode 1)))


(use-package undo-fu
	:ensure t
	:init
	(setq undo-limit 67108864) ; 64mb.
	(setq undo-strong-limit 100663296) ; 96mb.
	(setq undo-outer-limit 1006632960) ; 960mb.
)
 (define-key prog-mode-map [double-down-mouse-1] 'evil-jump-to-tag)
  (define-key evil-motion-state-map [down-mouse-1] nil)
  (define-key prog-mode-map [mouse-8] 'xref-pop-marker-stack)		 
;;; Below solves the mouse visual mode issue 
(progn
  (global-set-key [mouse-1] 'mouse-set-point)
  (global-unset-key [down-mouse-1])
  (global-unset-key [drag-mouse-1]))

 (use-package evil-collection
   :custom (evil-collection-setup-minibuffer t)
   :init (evil-collection-init)
   :ensure t)

 (use-package evil-matchit
   :ensure t
   :config (progn
     (global-evil-matchit-mode 1)))
;;; evil.el ends here


;;   (defun neotree-project-dir ()
;;     "Open NeoTree using the git root."
;;     (interactive)
;;     (let ((project-dir (projectile-project-root))
;;           (file-name (buffer-file-name)))
;;       (neotree-toggle)
;;       (if project-dir
;;           (if (neo-global--window-exists-p)
;;               (progn
;;                 (neotree-dir project-dir)
;;                 (neotree-find file-name)))
;;         (message "Could not find git project root."))))

;; ;; Disable line-numbers minor mode for neotree
;;   (add-hook 'neo-after-create-hook
;;             (lambda (&rest _) (display-line-numbers-mode -1)))
