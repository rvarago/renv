;; ============================ Project management ============================

(use-package projectile
  :commands projectile-mode
  :init
  (projectile-mode +1)
  :config
  (setq projectile-project-search-path '(("~/Projects/" . 3)))
  :bind
  (:map projectile-mode-map
        ("C-c p" . projectile-command-map)))

(use-package counsel-projectile
  :init (counsel-projectile-mode +1))
  ;; :config
  ;; (setq counsel-projectile-switch-project-action 'projectile-dired))

(use-package recentf
  :ensure nil
  :config
  (add-to-list 'recentf-exclude (format "%s/\\.emacs.d/early-init.el" (getenv "HOME")))
  (add-to-list 'recentf-exclude (format "%s/\\.emacs.d/init.el" (getenv "HOME")))
  (add-to-list 'recentf-exclude (format "%s/\\.emacs.d/elpa/.*" (getenv "HOME")))
  (add-to-list 'recentf-exclude (format "%s/\\.emacs.d/workspace/.*" (getenv "HOME")))
  (add-to-list 'recentf-exclude (format "%s/\\.local/lib/python3.9/site-packages/.*" (getenv "HOME")))
  (add-to-list 'recentf-exclude "/usr/lib/.*")
  (recentf-mode +1))
