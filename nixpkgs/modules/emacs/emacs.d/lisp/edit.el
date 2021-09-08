;; ============================ General editing settings ============================

(setq-default indent-tabs-mode nil)

(setq-default tab-width 4)
(setq-default tab-always-indent nil)

(put 'downcase-region 'disabled nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(defalias 'yes-or-no-p 'y-or-n-p)

(use-package hl-todo
  :config
  (add-to-list 'hl-todo-keyword-faces '("WIP" . "#94bff3"))
  (global-hl-todo-mode 1))

(use-package move-text
  :config (move-text-default-bindings))

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

(use-package undo-tree
  :init (global-undo-tree-mode))
