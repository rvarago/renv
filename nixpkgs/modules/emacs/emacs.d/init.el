;; ============================ Top-level grouping ============================

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(eval-when-compile
  (require 'use-package))

(use-package diminish
  :ensure t)

(load-file "~/.emacs.d/lisp/edit.el")
(load-file "~/.emacs.d/lisp/ui.el")
(load-file "~/.emacs.d/lisp/mini.el")
(load-file "~/.emacs.d/lisp/project.el")
(load-file "~/.emacs.d/lisp/git.el")
(load-file "~/.emacs.d/lisp/window.el")
(load-file "~/.emacs.d/lisp/term.el")
(load-file "~/.emacs.d/lisp/ide.el")
