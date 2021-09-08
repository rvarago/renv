;; ============================ Magit ============================

(use-package magit
  :bind (("C-x g" . magit-status)))

(use-package diff-hl
  :init (global-diff-hl-mode))

(use-package git-timemachine
  :bind (("C-x v t" . git-timemachine)))
