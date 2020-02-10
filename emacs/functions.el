(defun simpson-vertical-split()
  (interactive)
  (split-window-right)
  (evil-window-right 1))

(defun simpson-reload-emacs-config()
 "Reload emacs config."
 (interactive)
 (load-file "~/dotfiles/emacs/.emacs"))

(defun simpson-comment ()
   "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)
        (next-line)))
