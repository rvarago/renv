;;; mini.el --- Minibuffer search/narrowing/completion.
;;; Commentary:
;; Set up minibuffer search/narrowing/completion.

(use-package ivy
  :init (ivy-mode)
  :custom
  (ivy-use-virtual-buffers t)
  (enable-recursive-minibuffers t)
  ;; No regex starting with ^.
  (ivy-initial-inputs-alist nil))

(use-package ivy-rich
  :after ivy
  :init (ivy-rich-mode))

(use-package all-the-icons-ivy
  :after ivy
  :init (add-hook 'after-init-hook 'all-the-icons-ivy-setup))

(use-package counsel
  :after ivy
  :config (counsel-mode)
  :bind (("C-x f" . 'counsel-fzf)
         (:map minibuffer-local-map ("C-r" . 'counsel-minibuffer-history))))

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

(use-package command-log-mode)

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.2))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))

(use-package rg
  :config (rg-enable-default-bindings))
