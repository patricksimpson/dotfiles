;;; package --- Summary
;;;
;;; Containing all the settings.
;;;
;;; Commentary:
;;;
;;; This file is adapted from adam simpson by
;;; patrick simspon (no relation).
;;;
;;; Code:

;; Bootstrap `use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;turn off toolbar
(menu-bar-mode -1)

(eval-when-compile
  (require 'use-package))

(org-babel-do-load-languages
 'org-babel-load-languages
  '( (shell . t)
     (js . t)
   )
)

(setq use-package-verbose t)

(show-paren-mode)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq visible-bell nil)

(setq ring-bell-function (lambda ()
    (invert-face 'mode-line)
    (run-with-timer 0.1 nil 'invert-face 'mode-line)
))

(global-set-key (kbd "C-SPC") nil)

(defvar simpson-dropbox-path ""
  "Variable that points to the proper Dropbox path.")

(cond
 ((file-exists-p "~/Dropbox (Personal)/") (setq simpson-dropbox-path "~/Dropbox (Personal)/"))
 ((file-exists-p "~/Dropbox/") (setq simpson-dropbox-path "~/Dropbox/")))

(use-package json-mode
  :ensure t
  :mode ("\\.json\\'" . json-mode))

(use-package all-the-icons
  :defer 1
  :ensure t
  :config (progn
    (unless (file-exists-p "~/Library/Fonts/all-the-icons.ttf")
      (all-the-icons-install-fonts t))
  )
)

(use-package ag
  :ensure t)

(use-package dired-filter
  :ensure t)

(use-package dizzee
  :ensure t
  :config (progn
    (defvar dz-projects "~/dotfiles/emacs/dz-projects.el")
    (if (file-exists-p dz-projects)
    (load-file dz-projects)))
)

(progn
  (defvar dz-projects "~/dotfiles/emacs/dz-projects.el")
  (if (file-exists-p dz-projects)
      (progn (message "*Found DZ Projects*")
      (load-file dz-projects))
    (ding)
    (message "dz projects file not found!"))
)

(use-package neotree
  :ensure t
  :config

  (evil-leader/set-key
    "z"  'neotree-toggle
    "n"  'neotree-project-dir)

  (setq projectile-switch-project-action 'neotree-projectile-action)
  (add-hook 'neotree-mode-hook
    (lambda ()
      (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
      (define-key evil-normal-state-local-map (kbd "I") 'neotree-hidden-file-toggle)
      (define-key evil-normal-state-local-map (kbd "z") 'neotree-stretch-toggle)
      (define-key evil-normal-state-local-map (kbd "R") 'neotree-refresh)
      (define-key evil-normal-state-local-map (kbd "m") 'neotree-rename-node)
      (define-key evil-normal-state-local-map (kbd "c") 'neotree-create-node)
      (define-key evil-normal-state-local-map (kbd "d") 'neotree-delete-node)
      (setq-local linum-mode nil)
      (define-key evil-normal-state-local-map (kbd "s") 'neotree-enter-vertical-split)
      (define-key evil-normal-state-local-map (kbd "S") 'neotree-enter-horizontal-split)

      (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter))))

(setq neo-theme 'icons)
(use-package diminish
  :ensure t
  :config (progn
    (diminish 'undo-tree "")
    (diminish 'web-mode "")
    (diminish 'auto-revert "")
    (diminish 'smerge-mode "")
    (diminish 'flyspell-mode "spell")))

;; Awesome, deleting things goes to osx trash instead of `/dev/null`
(use-package osx-trash
  :if (eq system-type 'darwin)
  :ensure t
  :config (progn
    (osx-trash-setup)
    (setq delete-by-moving-to-trash t)))


(use-package doom-modeline
      :ensure t
      :defer t
      :init (add-hook 'after-init-hook #'doom-modeline-mode))

(use-package doom-themes
  :ensure t
  :init (load-theme 'doom-tomorrow-night t))

(use-package base16-theme
  :ensure t
  :init (load-theme 'base16-twilight t))


(require 'highlight-indentation)
(require 'smartparens-config)

(defvar psimpson-fixed-colors nil "Used to flag if the color fix has been applied.")

(defun psimpson-fix-colors()
  (interactive)
    (message "fixed colors")
    (setq psimpson-fixed-colors t)
    (set-face-attribute 'mode-line nil
        :background "#222222"
        :foreground "#777777"
        :box '(:line-width 3 :color "#1d1d1d" :style nil))
    (set-face-attribute 'mode-line-inactive nil
        :box '(:line-width 3 :color "#1d1d1d" :style nil))
    (set-face-background 'highlight-indentation-face "#222222")
    (set-face-background 'highlight-indentation-current-column-face "#323232")
    (set-face-foreground 'vertical-border "#323536")
    (set-face-background 'fringe "#1D1D1D"))

(use-package exec-path-from-shell
  :ensure t
  :config (progn
    ;(setq exec-path-from-shell-arguments "-l");remove -i
    ;causes error in shell
    (when (memq window-system '(mac ns))
      (exec-path-from-shell-initialize))))

(use-package dired-narrow
  :ensure t
  :demand
  :bind (:map dired-mode-map
              ("/" . dired-narrow-fuzzy)))

(use-package dired-subtree
  :ensure t
  :demand
  :bind (:map dired-mode-map
              ("i" . dired-subtree-toggle)))

(use-package vimish-fold
  :ensure t
  :config (vimish-fold-global-mode 1)
  :bind (("C-SPC v" . vimish-fold)
         ("C-SPC V" . vimish-fold-delete)))

(use-package flycheck
  :ensure t
  :diminish "lint"
  :defer 1
  :init (add-hook 'after-init-hook #'global-flycheck-mode)
  :bind ("C-SPC '" . flycheck-mode)
  :config (progn
            (advice-add 'flycheck-eslint-config-exists-p :override (lambda() t))))

(use-package tide
  :pin melpa-stable
  :ensure t
  :after flycheck
  :defer 1
  :config (progn
           (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)))

(use-package evil
  :ensure t
  :diminish "vim"
  :bind ("C-u" . evil-scroll-up)
  :init (progn
    (use-package evil-leader
      :ensure t
      :config (progn
        (global-evil-leader-mode)
        (evil-leader/set-leader ","))))
  :config (progn
    (evil-mode 1)
    (add-to-list 'evil-emacs-state-modes 'dired-mode)
    ;http://spacemacs.org/doc/FAQ#orgheadline31
    (fset 'evil-visual-update-x-selection 'ignore)
    (setq-default evil-shift-width 2)
    (setq evil-vsplit-window-right t)
    (setq evil-split-window-below t)
    (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
    (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
    (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
    (define-key evil-normal-state-map (kbd "gx") 'browse-url)
    (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
    (define-key evil-normal-state-map "\C-p" 'helm-projectile-find-file)
    (define-key evil-normal-state-map (kbd "C-b") 'projectile-switch-project)
    (evil-leader/set-key "f" 'helm-projectile-ag)
    (evil-leader/set-key "F" 'helm-do-ag)
    (evil-leader/set-key "c" 'fci-mode)
    (evil-leader/set-key "v" 'evil-window-vnew)
    (evil-leader/set-key "x" 'evil-window-new)
    (evil-leader/set-key "h" 'highlight-indentation-current-column-mode)
    (evil-leader/set-key "r" 'redraw-display)
    (evil-leader/set-key "l" 'display-line-numbers-mode)
    (evil-ex-define-cmd "W" 'save-buffer)
    (use-package key-chord
      :ensure t
      :config (progn
        (key-chord-mode 1)
        (setq key-chord-two-keys-delay 0.1)
        (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
        (key-chord-define-global "//" 'comment-region)
        (key-chord-define-global "??" 'uncomment-region)))))

(key-chord-mode 1)
(setq key-chord-two-keys-delay 0.1)

(use-package evil-matchit
  :ensure t
  :config (progn
    (global-evil-matchit-mode 1)
    (plist-put evilmi-plugins 'handlebars-mode '((evilmi-simple-get-tag evilmi-simple-jump)
      (evilmi-html-get-tag evilmi-html-jump)))))

(global-set-key (kbd "C-SPC d") (lambda() (interactive)
                                  (dired (concat simpson-dropbox-path "Notes/") "-laGht")))

(define-key dired-mode-map "r" '(lambda() (interactive) (ag (file-truename dired-directory))))

(use-package helm
  :ensure t
  :diminish ""
  :bind (
    ("M-x" . helm-M-x)
    ("C-=" . helm-mini)
    ("C-SPC f" . helm-find-files)
    ("C-SPC k p" . simpson-projects-browser))
  :config (progn
    (helm-mode)
    (require 'helm-config)
    (set-face-background 'helm-ff-dotted-directory "#2b303b")
    (set-face-background 'helm-ff-dotted-symlink-directory "#2b303b")
    (set-face-foreground 'helm-ff-dotted-directory "#65737e")
    (set-face-foreground 'helm-ff-dotted-symlink-directory "#65737e")

    (use-package projectile
      :ensure t
      :diminish ""
      :config (progn
        (projectile-global-mode)
        (setq projectile-switch-project-action 'projectile-dired)
        (setq projectile-enable-caching nil)

        (use-package helm-projectile
          :ensure t
          :config (progn
            (helm-projectile-on)))))
    (use-package helm-ag
      :ensure t
      :defer t
      :config (progn
        (custom-set-variables
        '(helm-ag-base-command "ag --nocolor --nogroup"))))))

(use-package helm-flyspell
  :ensure t
  :diminish ""
  :bind ("C-SPC C" . helm-flyspell-correct))

(defun simpson-projects-browser()
  (interactive)
  (cd "~/projects/")
  (helm-find-files nil)
  (neotree-refresh))

(use-package magit
  :ensure t
  :bind ("C-SPC g" . magit-status)
  :config (progn
    ;https://github.com/magit/magit/pull/2513
    ;Users who use Tramp and experience delays, should consider setting
    ;the option to `magit-auto-revert-repository-buffer-p'.
    ;; (setq auto-revert-buffer-list-filter 'magit-auto-revert-repository-buffers-p)
    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
    (set-face-foreground 'magit-blame-date "#ebcb8b")
    (set-face-foreground 'magit-blame-hash "#ebcb8b")
    (set-face-foreground 'magit-blame-heading "#ebcb8b")
    (set-face-foreground 'magit-blame-name "#ebcb8b")
    (set-face-foreground 'magit-blame-summary "#ebcb8b")
    (set-face-foreground 'magit-sequence-onto "#ebcb8b")
    (set-face-foreground 'magit-sequence-done "#ebcb8b")

    (set-face-foreground 'magit-hash "#96b5b4")
    (set-face-background 'magit-section-highlight "#343d46")

    (use-package evil-magit
      :ensure t)))

(use-package diff-hl
  :ensure t
  :bind (
    ("C-SPC r" . diff-hl-revert-hunk)
    ("C-x p" . diff-hl-previous-hunk)
    ("C-x n" . diff-hl-next-hunk))
  :config (progn
    (global-diff-hl-mode)
    (set-face-background 'diff-hl-change "#96b5b4")
    (set-face-background 'diff-hl-insert "#a3be8c")
    (set-face-background 'diff-hl-delete "#d08770")))

(use-package auto-complete
  :ensure t
  :diminish ""
  :config (progn
    (ac-config-default)
    ;this prevents the stupid behavior in scss where &:before {___ autocompletes
    ;lang!
    (defconst ac-css-pseudo-classes nil)))

(use-package org
  :defer 2
  :if (file-exists-p (concat simpson-dropbox-path "org/tasks.txt"))
  :ensure t
  :bind (
    ("C-SPC c" . simpson-org-task-capture)
    ("C-SPC t" . org-todo-list)
    ("C-SPC a" . org-agenda)
    ("C-SPC T" . org-tags-view))
  :mode (
    ("\\.txt\\'" . org-mode))
  :config (progn
    ;org mode location
    (setq org-agenda-file-regexp "\\`[^.].*\\.txt\\'")
    (setq org-agenda-files `(,(concat simpson-dropbox-path "org")))
    (setq org-log-done t)
    (setq org-deadline-warning-days 3)
    (setq org-agenda-custom-commands '(("n" "Agenda and all TODOs"
                                                ((agenda "")
                                                 (alltodo "")))
                                               ("w" "Work tasks" ((agenda "") (todo ""))
                                                ((org-agenda-files `(,(concat simpson-dropbox-path "org/tasks.txt")))))
                                               ("s" "side projects" ((agenda "") (todo ""))
                                                ((org-agenda-files `(,(concat simpson-dropbox-path "org/side.txt")))))))
    (setq org-todo-keywords
          '((sequence "TODO" "IN-PROGRESS" "WAITING" "|" "DONE" "CANCELED")))

    ;capture template [[http://blog.aaronbieber.com/2016/01/30/dig-into-org-mode.html][org caputre]]
    (setq org-capture-templates
          `(("a" "General Tasks" entry
            (file ,(concat simpson-dropbox-path "org/tasks.txt"))
"* TODO %? %^g
:PROPERTIES:
:CREATED: %T
:END:")
          ("s" "Side Projects" entry
            (file ,(concat simpson-dropbox-path "org/side.txt"))
"* TODO %? %^g
:PROPERTIES:
:CREATED: %T
:END:")
          ("p" "Personal tasks" entry
            (file ,(concat simpson-dropbox-path "org/personal.txt"))
"* TODO %? %^g
:PROPERTIES:
:CREATED: %T
:END:"))
)
    (setq org-refile-use-outline-path 'file)
    ;restore windows after org-todo-list closes
    (setq org-agenda-restore-windows-after-quit t)
    (add-hook 'org-mode-hook (lambda () (flyspell-mode 1)))
    ;org theme changes for ocean 16
    (set-face-foreground 'org-link "#a3be8c")
    (set-face-foreground 'org-tag "#ebcb8b")
    (set-face-foreground 'org-agenda-structure "#65737e")
    (setq org-html-head "
      <style>
        body {
          width: 800px;
          margin: 0 auto;
          font-family: sans-serif;
        }
        img {
          display: block;
          width: 100%;
          max-width: 100%;
        }
      </style>
    ")
    (setq exec-path (append exec-path '("/Library/TeX/texbin/latex")))
    (defun simpson-org-task-capture ()
      "Capture a task with my default template."
      (interactive)
      (org-capture nil "a"))
    (global-set-key (kbd "C-SPC k f") 'org-footnote-new)
    (global-set-key (kbd "C-SPC k l") 'org-toggle-link-display)))

(use-package multi-term
  :ensure t
  :config (progn
    (setq multi-term-program "/bin/bash")
    (setq multi-term-program-switches "--login")
    (define-key global-map (kbd "C-SPC p") 'term-paste)))

(use-package js2-mode
  :ensure t
  :diminish "JS"
  :interpreter (("node" . js2-mode))
    :config (progn
    (setq js2-basic-offset 2)
    (setq js2-highlight-level 3)
    (setq js2-bounce-indent-p t)
    (electric-indent-mode -1)
    (setq js2-mode-show-strict-warnings nil)
    (add-hook 'js2-mode-hook '(lambda() (setq show-trailing-whitespace t)))

    (global-set-key (kbd "C-SPC k j") 'js2-mode-hide-warnings-and-errors)
    (defcustom js2-strict-missing-semi-warning nil
      "Non-nil to warn about semicolon auto-insertion after statement.
    Technically this is legal per Ecma-262, but some style guides disallow
    depending on it."
      :type 'boolean
      :group 'js2-mode)))

;; (add-hook 'js2-mode-hook #'setup-tide-mode)

(use-package flow-minor-mode
  :ensure t)

(use-package rjsx-mode
  :ensure t
  :interpreter (("node" . rjsx-mode))
  :mode (("\\.js\\'" . rjsx-mode)
         ("\\.jsx?\\'" . rjsx-mode))
  :config (progn
            (setq js2-basic-offset 2)
            (setq js2-highlight-level 3)
            (setq js2-bounce-indent-p t)
            (electric-indent-mode -1)
            (setq js2-mode-show-strict-warnings nil)
            (add-hook 'js2-mode-hook 'flow-minor-enable-automatically)
            (add-hook 'js2-mode-hook (lambda() (setq show-trailing-whitespace t)))
            (add-hook 'rjsx-mode-hook 'flow-minor-enable-automatically)
            (add-hook 'rjsx-mode-hook 'prettier-js-mode)
            (add-hook 'js2-mode-hook 'prettier-js-mode)
            (add-hook 'web-mode-hook 'prettier-js-mode)
            (add-hook 'rjsx-mode-hook (lambda() (setq mode-name "jsx")))))

;indents! so brutal, each mode can have their own, e.g. css
;spaces
(setq-default indent-tabs-mode nil)

;2 of em
(setq-default tab-width 2)

;yes, css, even you
(setq-default css-indent-offset 2)

;yes, ts, even you
(setq-default typescript-indent-level 2)

;fonts
(set-face-attribute 'default nil :font "Monaco-12")
(set-frame-font "Monaco-12" nil t)


(use-package yaml-mode
  :ensure t
)

(use-package web-mode
  :ensure t
)

(require 'yaml-mode)
(require 'web-mode)

;modes w/ file extensions
(add-to-list 'auto-mode-alist '("\\.hbs?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ejs?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . css-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.js?\\'" . rjsx-mode))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;each line gets one line
(set-default 'truncate-lines t)

;backups suck, use Git
(setq make-backup-files nil) ; stop creating backup~ files
;backups suck, use Git
(setq auto-save-default nil)

;splash screen is gross
(setq inhibit-splash-screen t)

;; Use monospaced font faces in current buffer
(defun markdown-fonts ()
  (interactive)
  (setq buffer-face-mode-face '(:family "Monoco" :height 120))
  (buffer-face-mode))
;¯\_(ツ)_/¯
(setq flyspell-issue-message-flag nil)

;visual-fill-column
;https://github.com/joostkremers/visual-fill-column/blob/master/visual-fill-column.el
(add-hook 'visual-line-mode-hook 'visual-fill-column-mode)
(setq-default visual-fill-column-width 160)

;https://joelkuiper.eu/spellcheck_emacs
;brew install hunspell

;(add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell)


;treat new buffers as modified files
;http://stackoverflow.com/a/2592558/2344737
(add-hook 'find-file-hooks 'assume-new-is-modified)
(defun assume-new-is-modified ()
  (when (not (file-exists-p (buffer-file-name)))
    (set-buffer-modified-p t)))


;http://whattheemacsd.com/appearance.el-01.html
;; (defmacro rename-modeline (package-name mode new-name)
;;   `(eval-after-load ,package-name
;;      '(defadvice ,mode (after rename-modeline activate)
;;         (setq mode-name ,new-name))))

(defun simpson-header ()
  (setq header-line-format
    (list
      ;16 characters = /Users/patrick
      (if (stringp (buffer-file-name))
        (eval (concat " ▼ ../" (substring (buffer-file-name) 16 nil)))
      ":: empty ::"
      ))))

;set the header intially
(simpson-header)

;update the header whenever the buffer-list changes
(add-hook 'buffer-list-update-hook 'simpson-header)

;change header line color to match ocean dark
(set-face-foreground 'header-line "#a3adb5")
(set-face-background 'header-line "#323536")

(set-face-attribute 'header-line nil
    :box '(:line-width 1 :color "#323536" :style nil))

;y over yes
;http://pages.sachachua.com/.emacs.d/Sacha.html#orgheadline15
(fset 'yes-or-no-p 'y-or-n-p)

(defvar simpson-project-name()
  (:eval (when (ignore-errors (projectile-project-root))
    (concat " / " (projectile-project-name))))
)

(setq-default mode-line-format (list
  ;mode-line-modified
  '(:eval (if (buffer-modified-p)
      (propertize " !" 'face '(:foreground "#cf6a4c"))
  ))
  " "
  '(:eval (propertize evil-mode-line-tag 'face '(:foreground "#bf616a")))
  " "
  mode-line-position
  mode-line-modes
  mode-line-misc-info
))

(column-number-mode)

(use-package git-ps1-mode
  :ensure t)
  ;;:config (setq git-ps1-mode-lighter-text-format " [%s]"))

(git-ps1-mode)

(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

(setq confirm-kill-emacs 'yes-or-no-p)

(add-hook 'css-mode-hook '(lambda() (setq show-trailing-whitespace t)))
(set-face-background 'trailing-whitespace "#ab7967")
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package which-key
  :ensure t
  :diminish ""
  :config (which-key-mode)
)

(use-package fill-column-indicator
  :ensure t
  :config (setq fci-rule-column 80)
)

(use-package avy
  :ensure t
  :bind (
    ("C-SPC j" . avy-goto-word-1)
    ("C-SPC J" . avy-goto-char)
  )
  :config (progn
    (set-face-background 'avy-lead-face  "#bf616a")
    (set-face-background 'avy-lead-face-0  "#96b5b4")
    (set-face-background 'avy-lead-face-1  "#c0c5ce")
    (set-face-background 'avy-lead-face-2  "#b48ead")
  )
)

(use-package reveal-in-osx-finder
  :ensure t
)
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :bind ("C-SPC e" . yas-expand)
  :load-path "~/.emacs.d/elpa/yasnippet"
  :init (progn
    (add-hook 'rjsx-mode-hook #'yas-minor-mode)
    (add-hook 'js2-mode-hook #'yas-minor-mode)
    (add-hook 'org-mode-hook #'yas-minor-mode)
  )
  :config (progn
    (yas-reload-all)
    (define-key yas-minor-mode-map (kbd "<tab>") nil)
    (define-key yas-minor-mode-map (kbd "TAB") nil)
  )
)
(yas-global-mode 1)

(use-package emmet-mode
  :ensure t
  :diminish "emmet-mode"
  :bind (
    ("C-c e" . emmet-expand-line)
    ("C-c y" . emmet-next-edit-point)
  )
  :init (progn
    (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
    (add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
    ;JSX gets className not class
    (add-hook 'rjsx-mode-hook 'jsxEmmet)
    (add-hook 'js2-mode-hook 'jsxEmmet)
    (add-hook 'handlebars-mode-hook 'jsEmmet)
  )
  :config (progn
    (setq emmet-move-cursor-between-quotes t) ;; default nil
  )
)

(use-package alert
  :ensure t
  :config (progn
    (setq alert-default-style 'notifier)
    (defun alert-notifier-notify (info)
      (if alert-notifier-command
          (let ((args
                (list "-title"   (alert-encode-string (plist-get info :title))
                      "-message" (alert-encode-string (plist-get info :message))
                )))
            (message "%s" args)
            (apply #'call-process alert-notifier-command nil nil nil args))
        (alert-message-notify info)))
  )
)

; (use-package sauron
;   :pin melpa-stable
;   :ensure t
;   :defer 2
;   :init (setq sauron-modules '(sauron-erc))
;   :config (progn
;     (setq sauron-watch-nicks nil)
;     (setq sauron-hide-mode-line t)
;     (setq sauron-separate-frame t)
;     (setq sauron-column-alist '((timestamp . 8)
;       (origin . 7)
;       (message)))
;     (setq sauron-timestamp-format "%H:%M:%S")
;     (sauron-start-hidden)
;     ;(advice-add 'shell-command-sentinel :before #'simpson-shell-command-sentiel)
;   )
; )

(defun jsxEmmet()
  (setq emmet-expand-jsx-className? t)
)

(defun jsEmmet()
  (setq emmet-expand-jsx-className? nil)
)

(setq tramp-default-method "ssh")

;http://emacs.stackexchange.com/a/58
;to open a file with sudo, invoke Tramp C-x C-f and then type /sudo::/path

(setq dired-recursive-deletes t)
(setq delete-by-moving-to-trash t)
(setq dired-use-ls-dired nil)

(use-package editorconfig
  :ensure t
  :init (editorconfig-mode 0)
)

; (erc-log-mode)

; (use-package erc
;   :ensure t
;   :config (progn
;     (setq erc-default-port 6667)
;     (setq erc-prompt-for-password nil)
;     (setq erc-kill-queries-on-quit t)
;     (setq erc-log-write-after-send t)
;     (setq erc-log-insert-log-on-open t)
;     (setq erc-log-channels-directory "~/.erc/logs/")
;     (setq erc-save-buffer-on-part t)
;     (setq erc-join-buffer "bury")
;     (load-library "~/dotfiles/.emacs.d/irc-accounts.gpg")
;   )
; )
;
; (use-package flyspell
;   :defer 1
;   :config (progn
;   (add-hook 'erc-mode-hook (lambda () (flyspell-mode 1)))
;   (setq flyspell-issue-message-flag nil))
; )
;
; (setq auth-sources '("~/dotfiles/.emacs.d/authinfo.gpg"))
;
; (add-to-list 'load-path "~/.emacs.d/erc/")

; (require 'erc-slack-log)
; (erc-slack-log-enable)

;; (setq neo-autorefresh t)

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (setq tide-tsserver-executable "~/bin/tsserver")
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
;; (add-hook 'before-save-hook 'tide-format-before-save)
(add-hook 'typescript-mode-hook #'setup-tide-mode)
(add-hook 'rjsx-mode-hook #'setup-tide-mode)
(add-hook 'js2-mode-hook #'setup-tide-mode)

(use-package company
  :ensure t)

(use-package ivy
  :ensure t)

(use-package ivy-window-configuration
  :ensure nil
  :if (file-exists-p "~/dotfiles/emacs/ivy-window-configuration/")
  :load-path "~/dotfiles/emacs/ivy-window-configuration/")

(defun simpson-list-notes(search)
  "open notes directory"
  (interactive "sSearch Notes: ")
  (ag-dired (concat simpson-dropbox-path "Notes/") search)
)

(defun simpson-new-note(name)
  "Create new file for nvAlt with NAME."
  (interactive "sName of file: ")
  (let* ((use-buf (y-or-n-p "Use this buffer? "))
         (date (replace-regexp-in-string "\n$" "" (shell-command-to-string "date +%m-%d-%y")))
         (file (concat simpson-dropbox-path "Notes/" date "-" name ".txt")))
    (if use-buf
        (write-file file)
      (write-region "" "" file)
      (find-file file)
      )))

(use-package hydra
  :ensure t
  :defer 1
  :config (progn
            (global-set-key (kbd "C-SPC o") 'hydra-js2/body)
            (global-set-key (kbd "C-SPC z") 'ivy-window-configuration--hydra/body)
            (global-set-key (kbd "C-SPC v") 'hydra-vimish/body)
            (global-set-key (kbd "C-SPC x") 'hydra-origami/body)
            (global-set-key (kbd "C-SPC ?") 'hydra-help/body)
            (global-set-key (kbd "C-SPC m") 'hydra-js-modes/body)
            (evil-leader/set-key "e" 'hydra-command-line/body)
            (global-set-key (kbd "C-SPC s") 'hydra-emacs-settings/body)
            (evil-leader/set-key "i" 'hydra-insert-text/body)
            (evil-leader/set-key "b" 'hydra-blogger/body)
            (evil-leader/set-key "d" 'hydra-deploy/body)
            (evil-leader/set-key "a" 'hydra-org/body)
            (evil-leader/set-key "o" 'hydra-org/body)
            (evil-leader/set-key "s" 'hydra-emacs-settings/body)
            (evil-leader/set-key "p" 'hydra-robe/body)))

(defhydra hydra-org (:exit t)
  "
    ORG MODE
    _a_ agenda
    _l_ agenda-list
    _n_ new note
    _s_ search notes
    _t_ new task
    _f_ narrow focus
    _u_ unnarrow focus
  "
  ("a" org-agenda)
  ("l" org-agenda-list)
  ("n" simpson-new-note)
  ("s" simpson-list-notes)
  ("t" org-capture)
  ("f" org-narrow-to-subtree)
  ("u" widen))

(defhydra hydra-js2 ()
  "
    JS2 Folding/Narrowing:
    _n_ narrow to defun
    _v_ fold
    _d_ highlight defun
  "
  ("n" js2-narrow-to-defun "narrow to defun" :exit t)
  ("v" vimish-fold "fold")
  ("d" js2-mark-defun "highlight defun"))

(defhydra hydra-vimish (:exit t)
  "
    Vimish Folding
    _v_ fold
    _V_ unfold
    _x_ delete all folds
  "
  ("V" vimish-fold-delete "unfold")
  ("v" vimish-fold "fold")
  ("x" vimish-fold-delete-all "delete all"))

(defhydra hydra-tide (:exit t)
  "
    Tide Options
    _d_ documentation-at-point
    _J_ Add jsDoc at point
  "
  ("d" tide-documentation-at-point)
  ("J" tide-jsdoc-template))

(defhydra hydra-emacs-settings (:exit t)
  "
    Emacs Settings
    _e_ edit emacs file
    _k_ edit keybinds file
    _s_ edit settings file
    _r_ reload all settings
    _b_ eval current buffer
  "
  ("s" open-my-settings-file)
  ("k" open-my-keybinds-file)
  ("e" open-my-init-file)
  ("r" reload-my-config)
  ("b" eval-buffer)
)

(defhydra hydra-origami (:exit t)
  "
    Folding
    _o_ open node
    _c_ close node
    _s_ show only node
    _n_ next origami
    _p_ previou origami
    _u_ undo origami
    _r_ redo origami
    _x_ REMOVE ALL FOLDS
  "
  ("o" origami-open-node)
  ("c" origami-close-node)
  ("s" origami-show-only-node)
  ("n" origami-next-fold)
  ("p" origami-previous-fold)
  ("u" origami-undo)
  ("r" origami-redo)
  ("x" origami-reset)
)

(defun open-my-keybinds-file()
 "Open the keybinds file."
  (interactive)
  (find-file "~/dotfiles/emacs/keybinds.el"))

(defun open-my-settings-file()
 "Open the settings file."
  (interactive)
  (find-file "~/dotfiles/emacs/settings.el"))

(defun open-my-init-file()
 "Open the init file."
  (interactive)
  (find-file "~/dotfiles/emacs/.emacs"))

(defun reload-my-config()
 "reload config"
 (interactive)
 (load-file "~/dotfiles/emacs/.emacs"))

(defhydra hydra-help (:exit t)
  "
    Emacs - Describe
    _v_ describe variables
    _f_ describe function
    _s_ describe symbol
    _m_ describe mode
  "
  ("v" describe-variable "describe variable")
  ("f" describe-function "describe function")
  ("s" describe-symbol "describe symbol")
  ("m" describe-mode "describe mode"))


(defhydra hydra-js-modes (:exit t)
  "
    Switch Modes
    _r_ rjsx mode
    _j_ js2 mode
  "
  ("r" rjsx-mode)
  ("j" js2-mode))

(custom-set-faces
 '(ediff-current-diff-A ((t (:foreground "Red" :background "Black"))))
 '(ediff-current-diff-B ((t (:foreground "Green" :background "Black"))))
 '(ediff-current-diff-C ((t (:foreground "Yellow" :background "Black"))))
 '(ediff-even-diff-C ((t (:foreground "Yellow" :background "Black"))))
 '(ediff-odd-diff-C ((t (:foreground "Yellow" :background "Black"))))
 '(ediff-fine-diff-C ((t (:foreground "Yellow" :background "Black")))))

(server-start)

(use-package osx-clipboard
  :ensure t
  :defer 1
  :if (eq system-type 'darwin)
  :diminish "clip"
  :config (osx-clipboard-mode))

(defun patrick-currentfile (&optional count)
 (when (string-equal (buffer-name) " *NeoTree*")
    (message "%s" (neo-path--file-short-name (neo-buffer--get-filename-current-line)))))

(advice-add 'evil-previous-line :after #'patrick-currentfile)
(advice-add 'evil-next-line :after #'patrick-currentfile)

(defun shell-command-on-region-or-line ()
  (interactive)
  (async-shell-command

    (if (use-region-p)
        (buffer-substring (region-beginning) (region-end))
        (thing-at-point 'line t))))

;; this needs work..
(defun mutli-shell-command-on-region-or-line ()
  (interactive)
  (multi-term

    (if (use-region-p)
        (buffer-substring (region-beginning) (region-end))
        (thing-at-point 'line t))))

(defhydra hydra-command-line (:exit t)
  "
    _e_ run command in async shell
    _m_ run command in multi term
  "
  ("e" shell-command-on-region-or-line)
  ("m" multi-shell-command-on-region-or-line))

(defhydra hydra-insert-text (:exit t)
  "
    inserting text
    _i_ insert text at visual selection
    _r_ replace text at visual selection
  "
  ("i" string-insert-rectangle)
  ("r" string-rectangle))

(defhydra hydra-blogger (:exit t)
  "
    inserting text
    _n_ new post
    _d_ delete post
  "
  ("n" create-blog-post)
  ("d" delete-blog-post))

(defhydra hydra-deploy (:exit t)
  "
    inserting text
    _d_ deploy master
    _b_ branch deploy
  "
  ("d" deploy-master)
  ("b" deploy-ansible(simpson-deploy-branch)))

(defun deploy-master()
  (interactive)
  (deploy-ansible ("master")))

(defun deploy-branch(branch)
  (interactive)
  (list (read-string (format "Enter branch [%s]:" simpson-deploy-branch) nil nil simpson-deploy-branch))
  (setq simpson-deploy-branch branch)
  (message (concat "Deploying " simpson-deploy-branch)))

(defun deploy-ansible ()
  "Deploy ansible based projects."
  (interactive)
  (shell-command (concat "ansible-playbook playbook.yml --extraVars='revision=" simpson-deploy-branch "'")))

(defun create-blog-post ()
 "Prompt for the blog post name."
  (interactive)
  (let* ((title (read-string "Enter post title:"))
        (slug (replace-regexp-in-string " " "-" (downcase title))))
    (make-post-directory slug title)))

(defun make-post-directory(name title)
  "Make a post by NAME, TITLE."
  (interactive)
  (let* ((postdir (concat "~/posts/" (get-blog-year)))
         (npost (concat postdir "/" name ".md")))
    (shell-command (concat "mkdir -p " postdir))
    (shell-command (concat "touch " npost))
    (blog-make-meta title npost)
    (find-file npost)))

(defun blog-make-meta (title npost)
  (let*
         ((date (get-post-date))
          (a (get-string-from-file "~/posts/template.md"))
          (b (replace-in-string "_TITLE_" title a))
          (c (replace-in-string "_DATE_" date b)))
      (append-to-file c nil npost)))

(defun get-post-date ()
  "Get date from shell."
  (interactive)
  (concat
     (substring
      (shell-command-to-string "date \"+%Y-%m-%d %l:%M %p\"")
      0 -1)))

(defun get-blog-year ()
  "Get current year."
  (interactive)
  (substring
   (shell-command-to-string "date +%Y")
  0 -1))

(defun replace-in-string (what with in)
  (replace-regexp-in-string (regexp-quote what) with in 'literal))

(defun get-string-from-file (filePath)
  "Return FILEPATH's file content."
  (interactive)
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun image-to-text ()
  (interactive)
  (if buffer-file-name
     (progn
      (shell-command (concat "convert " buffer-file-name " -resize 400% -type Grayscale " buffer-file-name ".tif"))
      (shell-command (concat "tesseract -l eng " buffer-file-name ".tif " buffer-file-name))
      (shell-command (concat "rm " buffer-file-name ".tif"))
      (find-file (concat buffer-file-name ".txt")))))

(require 'iimage)
(autoload 'iimage-mode "iimage" "Support Inline image minor mode." t)
(autoload 'turn-on-iimage-mode "iimage" "Turn on Inline image minor mode." t)
(add-to-list 'iimage-mode-image-regex-alist '("@startuml\s+\\(.+\\)" . 1))

;; Rendering plantuml
(defun plantuml-render-buffer ()
  (interactive)
  (message "PLANTUML Start rendering")
  (shell-command (concat "java -jar ~/bin/plantuml.jar"
                         buffer-file-name))
  (message (concat "PLANTUML Rendered:  " (buffer-name))))

;; Image reloading
(defun reload-image-at-point ()
  (interactive)
  (message "reloading image at point in the current buffer...")
  (image-refresh (get-text-property (point) 'display)))

;; Image resizing and reloading
(defun resize-image-at-point ()
  (interactive)
  (message "resizing image at point in the current buffer123...")
  (let* ((image-spec (get-text-property (point) 'display))
         (file (cadr (member :file image-spec))))
    (message (concat "resizing image..." file))
    (shell-command (format "convert -resize %d %s %s "
                           (* (window-width (selected-window)) (frame-char-width))
                           file file))
    (reload-image-at-point)))

(use-package inf-ruby
  :ensure t)

(use-package rspec-mode
  :ensure t
  :defer 1
  :mode ("*spec.rb" . rspec-mode))

(defadvice rspec-compile (around rspec-compile-around)
  (let ((shell-file-name "/bin/bash"))
    ad-do-it))

(ad-activate 'rspec-compile)

(require 'prettier-js)

(use-package rubocopfmt
  :ensure t
  :diminish ":fire:"
  :init (progn
    ;; (add-hook 'ruby-mode-hook #'rubocopfmt-mode)
          (remove-hook 'ruby-mode-hook #'rubocopfmt-mode)
          (add-hook 'ruby-mode-hook 'display-line-numbers-mode)

  )
  )

(use-package projectile-rails
  :ensure t
)
(projectile-rails-global-mode)
(add-hook 'neo-after-create-hook (lambda (&optional dummy) (display-line-numbers-mode -1)))

(setq auto-mode-alist (cons '("Rakefile" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("Capfile" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("Gemfile" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.rake" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.god" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.rb" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.ru" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.gemspec" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.erb" . web-mode) auto-mode-alist))

(add-hook 'js2-mode-hook 'display-line-numbers-mode)
(add-hook 'web-mode-hook 'display-line-numbers-mode)
(add-hook 'ruby-mode-hook 'display-line-numbers-mode)
(add-hook 'css-mode-hook 'display-line-numbers-mode)
(add-hook 'json-mode-hook 'display-line-numbers-mode)
(add-hook 'rjsx-mode-hook 'display-line-numbers-mode)

(add-hook 'change-major-mode-hook #'psimpson-fix-colors)
(add-hook 'after-change-major-mode-hook  #'psimpson-fix-colors)

(add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)
(add-hook 'js-mode-hook #'smartparens-mode)
(add-hook 'ruby-mode-hook #'smartparens-mode)
(global-robe-mode)



(defhydra hydra-robe (:exit t)
  "
    robe mode
    _r_ start robe mode
    _d_ jump to definition
    _a_ ask for and jump to defintion
    _c_ rubocop format buffer
    _s_ go to spec file
  "
  ("r" robe-start)
  ("d" robe-jump)
  ("a" robe-ask)
  ("c" rubocopfmt)
  ("s" rspec-toggle-spec-and-target))


(use-package dired
  :hook (dired-mode . dired-hide-details-mode)
  :config
  ;; Colourful columns.
  (use-package diredfl
    :ensure t
    :config
    (diredfl-global-mode 1)))

(package-install-file "~/dotfiles/emacs/dired-git-info/dired-git-info.el")

(with-eval-after-load 'dired
  (define-key dired-mode-map ")" 'dired-git-info-mode))
;;; settings.el ends here