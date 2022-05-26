;;; fcs-python.el --- python development environment.

;;; Commentary:
;;; It mainly depends on the elpy package.

;;; Code:

(defun fcs-python-setup-h ()
  "Python mode setup hook."
  (elpy-enable)
  (hs-minor-mode +1)
  (projectile-mode +1)
  (display-line-numbers-mode +1)
  (highlight-indentation-mode -1))

(add-hook 'python-mode-hook 'fcs-python-setup-h)

(provide 'fcs-python)
;;; fcs-python.el ends here
