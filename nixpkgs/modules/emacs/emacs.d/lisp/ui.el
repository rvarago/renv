;;; ui.el --- User interface.
;;; Commentary:
;; Set up visual elements.

;; Maximize window on startup.
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(setq inhibit-startup-screen t)

;; Disable menu-bar and tool-bar.
(if (fboundp 'menu-bar-mode)
    (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))

;; Make *scratch* buffer blank.
(setq initial-scratch-message "")

;; Make window title the buffer name.
(setq-default frame-title-format '("%b"))

(set-frame-font "Iosevka-14")

;; Display line number except for certain modes.
;; TODO: Enable only for specific local modes rather than enabling globally then disabling locally.
(global-display-line-numbers-mode t) ; requires emacs 26.
(dolist (mode '(message-buffer-mode-hook
                treemacs-mode-hook
                magit-status-mode-hook
                org-mode-hook
                term-mode-hook
                vterm-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Display column number.
(column-number-mode)

;; Highlight cursor line.
(global-hl-line-mode +1)

;; Wrap at word boundaries.
(global-visual-line-mode 1)

;; Show parenthesis.
(show-paren-mode 1)
(setq show-paren-delay 0)

;; =============================== Theme ===============================
(use-package zenburn-theme
  :config
  (load-theme 'zenburn t))

(use-package doom-modeline
  :init (doom-modeline-mode 1))

;; ============================== All the icons ========================
(use-package all-the-icons)

;; ====================== Auto dim upon changing buffer ================
(use-package auto-dim-other-buffers
  :commands auto-dim-other-buffers-mode
  :diminish auto-dim-other-buffers-mode
  :init (auto-dim-other-buffers-mode))

;; ============================= Clickable links ========================
(use-package goto-addr
  :bind
  (:map goto-address-highlight-keymap
        ("C-c k" . goto-address-at-point))
  :hook
  ((eshell-mode shell-mode vterm-mode) . goto-address-mode)
  (prog-mode . goto-address-prog-mode))

