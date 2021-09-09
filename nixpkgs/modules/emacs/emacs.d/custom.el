;;; custom.el --- Customizations.
;;; Commentary:
;; Overall customizations.


(custom-set-variables
 '(rustic-ansi-faces
   ["black" "red3" "green3" "yellow3" "white" "magenta3" "cyan3" "white"])
 '(safe-local-variable-values '((rustic-test-arguments . "-- --skip integration"))))
(custom-set-faces
 '(rainbow-delimiters-depth-1-face ((t (:foreground "wheat"))))
 '(rainbow-delimiters-unmatched-face ((t (:background "dark gray" :foreground "red")))))
