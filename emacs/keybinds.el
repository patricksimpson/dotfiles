(global-set-key (kbd "C-M-u") 'universal-argument)
(global-set-key [f8] 'neotree-toggle)
(global-set-key (kbd "s-=") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)
(global-set-key (kbd "C-SPC k !") (lambda()
  (interactive)
  (text-scale-set 0)))


(setq window-saved "nothing")

(defun gen-multi-term ()
  (interactive)
  (switch-to-buffer-other-window nil)
  (multi-term)
)

(global-set-key (kbd "C-SPC k e") 'eval-region)
(global-set-key (kbd "C-SPC k t") 'gen-multi-term)
(global-set-key (kbd "C-SPC k r") 'revert-buffer)
(global-set-key (kbd "C-SPC k c") 'clone-indirect-buffer-other-window)
(global-set-key (kbd "C-SPC k v") 'visual-line-mode)
(global-set-key (kbd "C-SPC k i") 'erc-select)
(global-set-key (kbd "C-SPC k n") 'simpson-smart-shell)
(global-set-key (kbd "C-SPC k N") 'kill-shell-buffer)
(global-set-key (kbd "C-SPC !") 'async-shell-command)
(global-set-key (kbd "C-SPC u") 'universal-argument)
(global-set-key (kbd "C-SPC k z") 'neotree-toggle)
(global-set-key (kbd "C-SPC k d") (lambda()
  "shells out to date to return a formatted date string at point"
  (interactive)
  (shell-command "date +%Y-%m-%d-%I:%M" t)))

(global-set-key (kbd "C-SPC k D") (lambda(name)
  "create new file for Deft/nvAlt"
  (interactive "sName of file: ")
  (setq date (shell-command-to-string "date +%m-%d-%y"))
  (setq fixed-date (replace-regexp-in-string "\n$" "" date))
  (write-region "" "" (concat "~/Dropbox (Personal)/Notational Data/" fixed-date "-" name ".txt"))
))

(global-set-key (kbd "C-SPC /") 'swiper-helm)

(defun kill-shell-buffer()
  (interactive)
  (switch-to-buffer-other-window "*Async Shell Command*")
  (kill-buffer-and-window)
)

(defun simpson-smart-shell()
  (interactive)
  (unless (ignore-errors (projectile-run-async-shell-command-in-root))
  (async-shell-command))
)

(defun simpson-rerun()
  (interactive)
  (if (projectile-project-p)
    (projectile-with-default-dir (projectile-project-root)
      (async-shell-command (car shell-command-history)))
    (async-shell-command (car shell-command-history))
  )
)
(setq key-chord-two-keys-delay 1)
(key-chord-define evil-normal-state-map "gc" 'simpson-comment)

(global-set-key (kbd "C-SPC .") 'simpson-rerun)


;narrow region
(global-set-key (kbd "C-SPC n") 'narrow-to-region)
;widen
(global-set-key (kbd "C-SPC N") 'widen)


(defun simpson-comment ()
   "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)
        (next-line)))

;Move buffers with neotree-hide
(advice-add 'evil-window-move-far-right
            :before 'neotree-hide)

(advice-add 'evil-window-move-far-left
            :before 'neotree-hide)

;; (defun print-path ()
;;   "Print out current buffer path"
;;   (interactive)
;;   (message (buffer-file-name)))

;(global-set-key (kbd "S-SPC p") 'print-path)

;http://blog.aaronbieber.com/2016/01/30/dig-into-org-mode.html

(setq minor-mode-perm-list (copy-alist minor-mode-alist))

(setq minor-mode-alist (list))

;revert buffer to last-commit
(define-key global-map (kbd "C-SPC R") 'vc-revert)

(global-set-key "\M-h" 'help-command)

;write buffer to fil0
(define-key global-map (kbd "C-SPC w") 'write-file)
(define-key global-map (kbd "s-s") 'write-file)

(define-key global-map (kbd "C-SPC L") 'linum-mode)

(define-key global-map (kbd "s-t") nil)

(define-key global-map (kbd "C-x k") 'kill-buffer-and-window)


(define-key global-map (kbd "C-SPC E") 'simpson-erc)
(define-key global-map (kbd "C-SPC k E") 'simpson-kill-erc)

(defun simpson-erc()
  "loads all irc servers defined (as a list) in irc-accounts.gpg"
  (interactive)
  (seq-doseq (x sizzle-irc)
    (erc-tls :server x
      :nick (car (auth-source-user-and-password x))
      :password (cadr (auth-source-user-and-password x)))
  )
)

(defun simpson-freenode()
  "connect to freenode irc"
  (interactive)
  (let ((x "irc.freenode.net"))
    (erc :server x
      :nick (car (auth-source-user-and-password x))
      :password (cadr (auth-source-user-and-password x)))
  )
)

(defun simpson-kill-erc()
  "quits all erc servers"
  (interactive)
  (erc-cmd-GQ nil)
)

;; Dired mode additions (evil):
(define-key dired-mode-map "j" 'dired-next-line)
(define-key dired-mode-map "k" 'dired-previous-line)
(define-key dired-mode-map "r" 'helm-ag)
