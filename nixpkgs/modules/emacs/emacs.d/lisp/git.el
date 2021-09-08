;; ============================ Git integration ============================

(use-package magit
  :bind
  ("C-x g s" . magit-status)
  ("C-x g x" . magit-checkout)
  ("C-x g c" . magit-commit)
  ("C-x g p" . magit-push)
  ("C-x g f" . magit-fetch)
  ("C-x g u" . magit-pull)
  ("C-x g e" . magit-ediff-resolve)
  ("C-x g r" . magit-rebase)
  ("C-x g i" . magit-rebase-interactive)
  ("C-x g l" . magit-log))

(use-package diff-hl
  :init (global-diff-hl-mode))

(use-package git-timemachine
  :bind (("C-x g t" . git-timemachine)))
