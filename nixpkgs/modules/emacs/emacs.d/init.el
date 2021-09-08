;; ============================ Top-level grouping ============================

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(eval-when-compile
  (require 'use-package))

(use-package diminish)

;; TODO: Extract root from some constant (e.g. maybe with getenv).
(load-file "~/.emacs.d/lisp/edit.el")
(load-file "~/.emacs.d/lisp/git.el")
(load-file "~/.emacs.d/lisp/ide.el")
(load-file "~/.emacs.d/lisp/mini.el")
(load-file "~/.emacs.d/lisp/nav.el")
(load-file "~/.emacs.d/lisp/project.el")
(load-file "~/.emacs.d/lisp/term.el")
(load-file "~/.emacs.d/lisp/ui.el")
(load-file "~/.emacs.d/lisp/window.el")
