;;; init.el --- Base initialization.
;;; Commentary:
;; Set up base initialization.


;; Minimize garbage collection during startup
(setq gc-cons-threshold most-positive-fixnum)

;; Lower threshold back to 8 MiB (default is 800kB)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (expt 2 23))))

(defvar my-emacs-directory "~/.emacs.d/")

(load-file (concat my-emacs-directory "init-nix.el"))
;; (load-file (concat my-emacs-directory "init-nonnix.el"))

(setq custom-file (concat my-emacs-directory "custom.el"))
(load custom-file)

(defun my/load-file (file)
  "Load a elisp FILE from the Emacs directory."
  (load-file (concat my-emacs-directory "lisp/" file)))

(use-package diminish)

(my/load-file "edit.el")
(my/load-file "git.el")
(my/load-file "ide.el")
(my/load-file "mini.el")
(my/load-file "nav.el")
(my/load-file "project.el")
(my/load-file "term.el")
(my/load-file "ui.el")
(my/load-file "window.el")
