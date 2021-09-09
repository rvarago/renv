;;; project.el --- Project management.
;;; Commentary:
;; Set up project management.

(use-package projectile
  :hook (after-init . projectile-mode)

  :custom
  (projectile-completion-system 'ivy)
  (projectile-project-search-path '(("~/Projects/" . 3)))
  (projectile-enable-caching t)
  (projectile-switch-project-action #'projectile-dired)
  (projectile-mode-line '(:eval (projectile-project-name)))

  :config
  (projectile-global-mode)

  :bind
  (:map projectile-mode-map
        ("C-c p" . projectile-command-map)))

(use-package counsel-projectile
  :after (counsel projectile)
  :init (counsel-projectile-mode))

(use-package recentf
  :init (recentf-mode)
  :custom
  (recentf-exclude (list "COMMIT_EDITMSG"
                         "~$"
                         "/tmp/"))
  (recentf-max-menu-items 20))
