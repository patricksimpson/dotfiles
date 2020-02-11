;;; hyrda (best menu system on the planet)

(use-package hydra
  :ensure t
  :defer 1
  :config (progn
            (evil-leader/set-key "z" 'hydra-zoom/body)
            (evil-leader/set-key "i" 'hydra-insert-text/body)
            (evil-leader/set-key "x" 'hydra-ide/body)
            ;;; (evil-leader/set-key "b" 'hydra-blogger/body)
            ;;; (evil-leader/set-key "a" 'hydra-org/body)
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
  ("i" i-rai)
  ("r" robe-start)
  ("d" robe-jump)
  ("a" robe-ask)
  ("c" rubocopfmt)
  ("s" rspec-toggle-spec-and-target))

;;; hrydas.el ends here
