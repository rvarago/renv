;; ============================ Window management ============================

(use-package window
  :bind (("C-x 2" . vsplit-last-buffer)
         ("C-x 3" . hsplit-last-buffer)
         ;; Kill a buffer without asking.
         ([remap kill-buffer] . kill-this-buffer))
  :preface
  (defun hsplit-last-buffer ()
    "Focus to the last created horizontal window."
    (interactive)
    (split-window-horizontally)
    (other-window 1))

  (defun vsplit-last-buffer ()
    "Focus to the last created vertical window."
    (interactive)
    (split-window-vertically)
    (other-window 1)))

(use-package centered-window
  :bind (("C-x c w" . centered-window-mode))
  :custom
  (cwm-centered-window-width 130)
  (cwm-frame-internal-border 0)
  (cwm-incremental-padding t)
  (cwm-incremental-padding-% 2)
  (cwm-left-fringe-ratio 0)
  (cwm-use-vertical-padding t))

(use-package switch-window
  :bind (("C-x o" . switch-window)
         ("C-x w" . switch-window-then-swap-buffer)))

(use-package winner
  :hook (after-init . winner-mode))
