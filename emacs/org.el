(setq org-directory "~/notes")
(setq org-agenda-files (list
                        org-directory
                        "~/org/inbox.txt"
                        "~/org/home.txt"
                        "~/org/work.txt"))

(setq org-refile-targets
      '(("done.txt" :maxlevel . 1)
        ("home.txt" :maxlevel . 1)
        ("work.txt" :maxlevel . 1)))

(global-set-key (kbd "C-c c") 'counsel-org-capture)
(global-set-key (kbd "C-c t") 'org-tags-view)
(global-set-key (kbd "C-c a") 'counsel-org-agenda-headlines)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c r") 'org-refile)
(global-set-key (kbd "C-c q") 'org-archive-subtree)

;;; https://blog.aaronbieber.com/2016/01/30/dig-into-org-mode.html
(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "WAITING" "|" "DONE" "CANCELED")))

(setq org-capture-templates
      `(("a" "General Inbox" entry
         (file "~/org/inbox.txt")
"* TODO %? %^g
:CREATED: %T
:END:")
      ("h" "Home" entry
         (file "~/org/home.txt")
"* TODO %? %^g
:CREATED: %T
:END:")
        ("w" "Work" entry
         (file "~/org/work.txt")
"* TODO %? %^g
:CREATED: %T
:END:")))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (shell . t)
   (ruby . t)
   (js . t)
   (emacs-lisp . t)))
