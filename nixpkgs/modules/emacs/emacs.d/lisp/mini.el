;; ============================ Minibuffer completion settings ============================

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
  :after (ivy all-the-icons)
  :init (add-hook 'after-init-hook 'all-the-icons-ivy-setup))

(use-package counsel
  :after ivy
  :config (counsel-mode)
  :bind ((:map minibuffer-local-map ("C-r" . 'counsel-minibuffer-history))))

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))
