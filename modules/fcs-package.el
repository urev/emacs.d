;;; fcs-package.el --- package tools.
;;; Commentary:
;;; Code:

(require 'cl-lib)
(require 'package)

(defconst fcs-packages
  '(ag
    beacon
    ccls
    company
    clang-format
    doom-modeline
    evil
    flycheck
    helm
    helm-ag
    helm-projectile
    hl-todo
    lsp-mode
    neotree
    smartparens
    projectile
    which-key
    yasnippet
    yasnippet-snippets
    zenburn-theme
    company-go
    go-mode
    go-eldoc))

(defun fcs-package-get (pkg)
  "Install PKG if it does not exist."
  (unless (package-installed-p pkg)
    (package-install pkg)))

(defun fcs-package-setup ()
  "Install all packages in fcs-package."
  (unless (cl-every 'package-installed-p fcs-packages)
    (package-refresh-contents)
    (mapc 'fcs-package-get fcs-packages)))

(package-initialize)

(setq package-archives
      '(("gnu" . "http://mirrors.ustc.edu.cn/elpa/gnu/")
	("melpa" . "http://mirrors.ustc.edu.cn/elpa/melpa/")
	("melpa-stable" . "http://mirrors.ustc.edu.cn/elpa/melpa-stable/")
	("org" . "http://mirrors.ustc.edu.cn/elpa/org/")))

(setq package-user-dir fcs-elpa-dir)

(fcs-package-setup)

(provide 'fcs-package)
;;; fcs-package.el ends here
