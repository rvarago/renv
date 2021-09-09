;; ============================ Top-level grouping ============================

(defvar my-emacs-directory "~/.emacs.d/")

(load-file (concat my-emacs-directory "init-nix.el"))
;; (load-file (concat my-emacs-directory "init-nonnix.el"))

(use-package diminish)

(setq custom-file (concat my-emacs-directory "custom.el"))
(load custom-file)

(defun my/load-file (file)
  "Load a elisp FILE from the Emacs directory."
  (load-file (concat my-emacs-directory "lisp/" file)))

(my/load-file "edit.el")
(my/load-file "git.el")
(my/load-file "ide.el")
(my/load-file "mini.el")
(my/load-file "nav.el")
(my/load-file "project.el")
(my/load-file "term.el")
(my/load-file "ui.el")
(my/load-file "window.el")
