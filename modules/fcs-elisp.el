;;; fcs-elisp.el --- elisp development environment.
;;; Commentary:
;;; Code:

(require 'flycheck)

(defun fcs-elisp-setup-h ()
  "Elisp mode setup hook."
  (flycheck-mode +1)
  (hs-minor-mode +1)
  (display-line-numbers-mode +1))

(with-eval-after-load 'elisp-mode
  (add-hook 'emacs-lisp-mode-hook 'fcs-elisp-setup-h))

(provide 'fcs-elisp)
;;; fcs-elisp.el ends here
