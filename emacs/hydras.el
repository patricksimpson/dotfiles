;;; hyrda (best menu system on the planet)

(use-package hydra
  :ensure t
  :defer 1
  :config (progn
            (evil-leader/set-key "z" 'hydra-zoom/body)
            (evil-leader/set-key "i" 'hydra-insert-text/body)
            (evil-leader/set-key "x" 'hydra-ide/body)
            ;;; (evil-leader/set-key "b" 'hydra-blogger/body)
            (evil-leader/set-key "o" 'hydra-org/body)
            (evil-leader/set-key "s" 'hydra-spell/body)
            (evil-leader/set-key "p" 'hydra-robe/body)))

(defhydra hydra-ide (:exit t)
  "
   emacs ide settings
   _f_ flycheck
   _l_ line numbers
   _p_ show smart parens
  "
  ("f" flycheck-mode)
  ("l" display-line-numbers-mode)
  ("p" show-smartparens-global-mode))

(defhydra hydra-zoom (:exit t)
  "zoom"
  ("g" text-scale-increase "in")
  ("l" text-scale-decrease "out"))

(defhydra hydra-insert-text (:exit t)
  "
    inserting text
    _i_ insert text at visual selection
    _r_ replace text at visual selection
  "
  ("i" string-insert-rectangle)
  ("r" string-rectangle))

(defhydra hydra-spell (:exit t)
  "
    inserting text
    _s_ spell fix
    _r_ toggle flyspell-prog-mode 
    _t_ toggle flyspell-mode 
    _i_ ispell fix it
  "
  ("s" flyspell-correct-word-before-point)
  ("r" flyspell-prog-mode)
  ("t" flyspell-mode)
  ("i" ispell))

(defhydra hydra-robe (:exit t)
  "
    robe mode
    _i_ start inf-rails-console
    _r_ start robe mode
    _d_ jump to definition
    _a_ ask for and jump to defintion
    _c_ rubocop format buffer
    _s_ go to spec file
  "
  ("i" inf-ruby-console-rails)
  ("r" robe-start)
  ("d" robe-jump)
  ("a" robe-ask)
  ("c" rubocopfmt)
  ("s" rspec-toggle-spec-and-target))

(defhydra hydra-org(:exit t)
  "
    org mode
    _a_ [C-c a] org-agenda
    _c_ [C-c c] org-capture
    _l_ [C-c l] org-store-link
    _k_ [C-c k] copy-current-file-path
    _s_ [C-c s] search and list-notes
    _n_ [C-c n] new note
  "
  ("a" org-agenda)
  ("c" org-capture)
  ("l" org-store-link)
  ("k" simpson-copy-current-file-path)
  ("s" simpson-list-notes)
  ("n" simpson-new-note))
;;; hrydas.el ends here
