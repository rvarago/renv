;; ============================ General editing settings ============================

(setq-default indent-tabs-mode nil)

(setq-default tab-width 4)
(setq-default tab-always-indent nil)

(put 'downcase-region 'disabled nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Move line up.
(defun move-line-up ()
  "Move the current line up."
  (interactive)
  (transpose-lines 1)
  (previous-line 2))

(global-set-key (kbd "M-<up>") 'move-line-up)

;; Move line down.
(defun move-line-down ()
  "Move the current line down."
  (interactive)
  (next-line 1)
  (transpose-lines 1)
  (previous-line 1))

(global-set-key (kbd "M-<down>") 'move-line-down)
