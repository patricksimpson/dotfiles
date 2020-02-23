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

(defun simpson-copy-current-file-path ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))

(defun simpson-list-notes(search)
  "open notes directory"
  (interactive "sSearch Notes: ")
  (ag-dired "~/notes/" search))

(defun simpson-new-note(name)
  "Create new file for nvAlt with NAME."
  (interactive "sName of file: ")
  (let* ((use-buf (y-or-n-p "Use this buffer? "))
         (date (replace-regexp-in-string "\n$" "" (shell-command-to-string "date +%m-%d-%y")))
         (file (concat "~/notes/" date "-" name ".txt")))
    (if use-buf
        (write-file file)
      (write-region "" "" file)
      (find-file file))))
