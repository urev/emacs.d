;;; fcs-go.el --- golang development environment.

;;; Commentary:
;;; We need to install the following tools:
;;;  goimports, gofmt, gocode, godef, golint, godoc.

;;; Code:

(require 'company)
(require 'company-go)

(defun fcs-go-setup-h ()
  "Golang mode setup hook."
  (subword-mode +1)
  (hs-minor-mode +1)
  (projectile-mode +1)
  (helm-projectile-on)
  (display-line-numbers-mode +1)

  (go-eldoc-setup)
  (setq tab-width 4)
  (set (make-local-variable 'company-backends) '(company-go))

  (let ((goimports (executable-find "goimports")))
    (when goimports (setq gofmt-command goimports)))

  (add-hook 'before-save-hook 'gofmt-before-save nil t))

(with-eval-after-load 'go-mode
  (add-hook 'go-mode-hook 'fcs-go-setup-h))

(provide 'fcs-go)
;;; fcs-go.el ends here
