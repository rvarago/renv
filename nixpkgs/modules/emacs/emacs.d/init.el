;; ============================ Top-level grouping ============================

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(eval-when-compile
  (require 'use-package))

(load-file "~/.emacs.d/lisp/edit.el")
(load-file "~/.emacs.d/lisp/git.el")
(load-file "~/.emacs.d/lisp/ide.el")
(load-file "~/.emacs.d/lisp/ui.el")
