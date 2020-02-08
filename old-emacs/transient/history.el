((magit-branch nil)
 (magit-cherry-pick nil
                    ("--ff"))
 (magit-commit nil
               ("--no-verify"))
 (magit-diff
  ("--no-ext-diff" "--stat"))
 (magit-dispatch nil)
 (magit-fetch nil)
 (magit-log
  ("-n256" "--graph" "--decorate")
  ("-n256" "--graph" "--color" "--decorate")
  ("-n256"
   ("--" "NewInvoices")
   "--graph" "--color" "--decorate")
  ("-n256" "--author=Mike Kalvas <michael.kalvas@beam.dental>" "--graph" "--decorate"))
 (magit-log-refresh
  ("-n256" "--topo-order" "--graph" "--color" "--decorate"))
 (magit-pull
  ("--rebase"))
 (magit-push
  ("--force-with-lease")
  ("--force"))
 (magit-rebase
  ("--interactive")
  nil)
 (magit-reset nil)
 (magit-stash nil)
 (magit-tag nil)
 (magit-worktree nil)
 (magit:-- "NewInvoices" "")
 (magit:--author "Mike Kalvas <michael.kalvas@beam.dental>"))
