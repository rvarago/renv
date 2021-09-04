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

;; Delete line from point to begin.
(defun delete-line-to-begin ()
  "Deletes from point to end of line without backing up into 'kill-ring'."
  (interactive)
  (delete-region (line-beginning-position) (point)))

(global-set-key (kbd "C-<") 'delete-line-to-begin)

;; Delete line from point to end.
(defun delete-line-to-end ()
  "Deletes from point to end of line without backing up into 'kill-ring'."
  (interactive)
  (delete-region (point) (line-end-position)))

(global-set-key (kbd "C->") 'delete-line-to-end)
