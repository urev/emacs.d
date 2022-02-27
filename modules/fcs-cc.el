;;; fcs-cc.el --- c plus plus development environment.
;;; Commentary:
;;; Code:

(require 'ccls)
(require 'flycheck)
(require 'projectile)

(defun fcs-cc-reformat-h ()
  "Reformat buffer if .clang-format exists in the projectile root."
  (when (f-exists?
	 (expand-file-name ".clang-format" (projectile-project-root)))
    (clang-format-buffer)))

(defun fcs-cc-setup-h ()
  "Cpp development environment setup hook."
  (hs-minor-mode +1)
  (projectile-mode +1)
  (helm-projectile-on)
  (display-line-numbers-mode +1)

  (flycheck-mode +1)
  (setq flycheck-disabled-checkers
	'(c/c++-clang c/c++-cppcheck c/c++-gcc))

  (setq ccls-executable (executable-find "ccls"))
  (setq lsp-lens-enable nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (lsp)

  (setq c-basic-offset 4)
  (add-hook 'before-save-hook 'fcs-cc-reformat nil t))

(with-eval-after-load 'cc-mode
  (add-hook 'c-mode-common-hook 'fcs-cc-setup-h))

(provide 'fcs-cc)
;;; fcs-cc.el ends here
