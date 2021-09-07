;; ============================ Project management ============================

(use-package vterm
  :hook (vterm-mode . (lambda ()
                        (setq-local global-hl-line-mode nil)
                        (setq-local line-spacing nil)))
  :config
    (define-key vterm-mode-map (kbd "C-l") #'(lambda ()
                                             (interactive)
                                             (vterm-clear)
                                             (vterm-clear-scrollback))))

(use-package vterm-toggle
  :after (vterm projectile)
  :config
  (setq vterm-toggle-fullscreen-p nil)
  (setq vterm-toggle-scope 'project)
  (global-set-key (kbd "C-`") #'vterm-toggle)
  (add-to-list 'display-buffer-alist
               '((lambda (bufname _) (with-current-buffer bufname (equal major-mode 'vterm-mode)))
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 (direction . bottom)
                 (dedicated . t)
                 (reusable-frames . visible)
                 (window-height . 0.3))))

