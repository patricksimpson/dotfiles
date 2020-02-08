(defun simpson-vertical-split()
  (interactive)
  (split-window-right)
  (evil-window-right 1))

(defun simpson-reload-emacs-config()
 "Reload emacs config."
 (interactive)
 (load-file "~/dotfiles/emacs/.emacs"))
