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
            (evil-leader/set-key "w" 'hydra-describe/body)
            (evil-leader/set-key "m" 'hydra-import/body)
            (evil-leader/set-key "p" 'hydra-robe/body)))

(defhydra hydra-ide (:exit t)
  "
   emacs ide settings
   _f_ flycheck
   _l_ line numbers
  "
  ("f" flycheck-mode)
  ("l" display-line-numbers-mode))

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
    _v_ rspec-verify
    _x_ rspec-verify-all
    _c_ rubocop format buffer
    _p_ rubocop format mode toggle
    _s_ go to spec file
  "
  ("i" inf-ruby-console-rails)
  ("r" robe-start)
  ("d" robe-jump)
  ("a" robe-ask)
  ("v" rspec-verify)
  ("x" rspec-verify-all)
  ("c" rubocopfmt)
  ("p" rubocopfmt-mode)
  ("s" rspec-toggle-spec-and-target))

(defhydra hydra-org(:exit t)
  "
    org mode
    _a_ [C-c a] org-agenda
    _c_ [C-c c] org-capture
    _l_ [C-c l] org-store-link
    _r_ [C-c r] org-refile
    _q_ [C-c q] org-archive-subtree
    _k_ [C-c k] copy-current-file-path
  "
  ("a" org-agenda)
  ("c" counsel-org-capture)
  ("l" org-store-link)
  ("r" org-refile)
  ("q" org-archive-subtree)
  ("k" simpson-copy-current-file-path))


(defhydra hydra-describe(:exit t)
  "
    notes 
    _v_ describe-variable 
    _k_ describe-key
    _f_ describe-function 
    _m_ describe-mode  
  "
  ("v" describe-variable)
  ("k" describe-key)
  ("f" describe-function)
  ("m" describe-mode))

(defhydra hydra-import(:exit t)
  "
    notes 
    _f_ fix imports
    _i_ import js
    _m_ goto import
    _g_ goto import
    _s_ start import js
    _r_ run import js
  "
  ("f" import-js-fix)
  ("i" import-js-import)
  ("m" import-js-goto)
  ("g" import-js-goto)
  ("s" run-import-js)
  ("r" run-import-js))

(defhydra hydra-notes(:exit t)
  "
    notes 
    _s_ search notes [C-c s] 
    _l_ list notes
    _n_ new note [C-c n] 
  "
  ("s" simpson-search-notes)
  ("l" simpson-list-notes)
  ("n" simpson-new-note))
;;; hrydas.el ends here
