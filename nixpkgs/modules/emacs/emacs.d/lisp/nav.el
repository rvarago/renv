;; ============================ Navigation across trees/tabs  ============================

;; ======================== Dired ========================
(use-package all-the-icons-dired
  :after (dired all-the-icons)
  :hook (dired-mode . all-the-icons-dired-mode))

;; ======================== Treemacs ========================
(use-package treemacs
  :init
  (add-hook 'projectile-after-switch-project-hook 'treemacs-display-current-project-exclusively)
  :config
  (setq treemacs-show-cursor nil)
  (treemacs-load-theme "all-the-icons")
  :bind ([f9] . treemacs)
        ("M-0" . treemacs-select-window))

(use-package treemacs-all-the-icons
  :after treemacs)

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-magit
  :after (treemacs magit))

;; ======================== Centaur-Tabs ========================
(use-package centaur-tabs
  :config
  (centaur-tabs-mode t)
  :hook
  (eshell-mode . centaur-tabs-local-mode)
  :custom
  (centaur-tabs-gray-out-icons 'buffer)
  (centaur-tabs-style "rounded")
  (centaur-tabs-height 36)
  (centaur-tabs-set-icons t)
  (centaur-tabs-set-modified-marker t)
  (centaur-tabs-modified-marker "●")
  (centaur-tabs-buffer-groups-function centaur-tabs-projectile-buffer-groups)
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward))
