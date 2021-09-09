;;; init-nonnix.el --- Base initialization for non-nix systems.
;;; Commentary:
;; Set up base where packages aren't expected to be installed via nix.

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)

(require 'package)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Enable to enforce auto installation if a package isn't locally available.
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; Enable to obtain auto-updates with package enforcement.
(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

