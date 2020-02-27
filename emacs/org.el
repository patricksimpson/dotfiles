(setq org-directory "~/notes")
(setq org-agenda-files (list
                        org-directory
                        "~/org/inbox.txt"
                        "~/org/home.txt"
                        "~/org/work.txt"))

(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c t") 'org-tags-view)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c l") 'org-store-link)

;;; https://blog.aaronbieber.com/2016/01/30/dig-into-org-mode.html
(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "WAITING" "|" "DONE" "CANCELED")))

(setq org-capture-templates
      `(("a" "General Inbox" entry
         (file "~/org/inbox.txt")
"* TODO %? %^g
:CREATED: %T
:END:")))
