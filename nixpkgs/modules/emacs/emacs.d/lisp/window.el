;;; window.el --- Window management.
;;; Commentary:
;; Set up window management.

(use-package window
  :bind (("C-x 2" . vsplit-last-buffer)
         ("C-x 3" . hsplit-last-buffer)
         ;; Kill a buffer without asking.
         ([remap kill-buffer] . kill-this-buffer))
  :preface
  (defun hsplit-last-buffer ()
    "Focus to the last created horizontal window."
    (interactive)
    (split-window-horizontally)
    (other-window 1))

  (defun vsplit-last-buffer ()
    "Focus to the last created vertical window."
    (interactive)
    (split-window-vertically)
    (other-window 1)))

(use-package centered-window
  :bind (("C-x t c" . centered-window-mode))
  :custom
  (cwm-centered-window-width 130)
  (cwm-frame-internal-border 0)
  (cwm-incremental-padding t)
  (cwm-incremental-padding-% 2)
  (cwm-left-fringe-ratio 0)
  (cwm-use-vertical-padding t))

(use-package switch-window
  :bind (("C-x o" . switch-window)
         ("C-x w" . switch-window-then-swap-buffer)))

(use-package winner
  :hook (after-init . winner-mode))

(use-package shackle
  :hook (after-init . shackle-mode)
  :custom
  (shackle-default-size 0.5)
  (shackle-default-alignment 'below)
  (shackle-rules
   '((magit-log-mode       :select t :inhibit-window-quit t :same t)
     ("*quickrun*"         :select t :inhibit-window-quit t :same t)
     (profiler-report-mode :select t)
     (apropos-mode         :select t :align t :size 0.3)
     (help-mode            :select t :align t :size 0.4)
     (comint-mode          :select t :align t :size 0.4)
     (grep-mode            :select t :align t)
     (rg-mode              :select t :align t)
     (ag-mode              :select t :align t)
     ("*org-roam*"         :select nil :align t :inhibit-window-quit t :same t)
     ("*Flycheck errors*"         :select t   :align t :size 10)
     ("*Backtrace*"               :select t   :align t :size 15)
     ("*ydcv*"                    :select nil :align t :size 0.4)
     ("*Shell Command Output*"    :select nil :align t :size 0.4)
     ("*Async Shell Command*"     :select nil :align t :size 0.4)
     ("*Org-Babel Error Output*"  :select nil :align t :size 0.3)
     ("*package update results*"  :select nil :align t :size 10)
     ("*Process List*"            :select t   :align t :size 0.3)
     ("*Help*"                    :select t   :align t :size 0.3)
     ("*Occur*"                   :select t   :align right)
     ("\\*ivy-occur .*\\*"        :select t   :align right :regexp t)
     ("\\*eldoc\\( for \\)?.*\\*" :select nil :align t :size 15 :regexp t))))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  (setq uniquify-after-kill-buffer-p t)     ; Rename after killing uniquified.
  (setq uniquify-ignore-buffers-re "^\\*")) ; Don't muck with special buffers.

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))

(use-package ibuffer-projectile
  :after (ibuffer projectile)
  :preface
  (defun my/ibuffer-projectile ()
    (ibuffer-projectile-set-filter-groups)
    (unless (eq ibuffer-sorting-mode 'alphabetic)
      (ibuffer-do-sort-by-alphabetic)))
  :hook (ibuffer . my/ibuffer-projectile))

(defvar *protected-buffers* '("*scratch*" "*Messages*")
  "Buffers that cannot be killed.")

(defun my/protected-buffers ()
  "Protects some buffers from being killed."
  (dolist (buffer *protected-buffers*)
    (with-current-buffer buffer
      (emacs-lock-mode 'kill))))

(add-hook 'after-init-hook #'my/protected-buffers)
