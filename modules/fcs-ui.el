;;; fcs-ui.el --- emacs ui config.
;;; Commentary:
;;; Code:

(require 'beacon)
(require 'bookmark)
(require 'company)
(require 'doom-modeline)
(require 'helm)
(require 'helm-ag)
(require 'helm-projectile)
(require 'hl-todo)
(require 'projectile)
(require 'smartparens-config)
(require 'which-key)

;; Set up color theme.
(add-to-list 'default-frame-alist '(background-color . "black"))
(load-theme 'zenburn t)

;; Set up line number style.
(custom-set-faces
 '(line-number ((t (:background "black" :foreground "#6F6F6F")))))

;; Hide menu bar.
(menu-bar-mode -1)

;; Show column line.
(column-number-mode +1)

;; Highlight the `TODO' annotation.
(global-hl-todo-mode +1)

;; Shine the cursor when jumping.
(beacon-mode +1)

;; Set up the mode line style.
(doom-modeline-mode +1)

;; Show the shortcut when using a command.
(which-key-mode +1)

;; Set up for parentheses.
(smartparens-global-mode +1)

;; Delete trailing whitespace when saving a file.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Use helm for incremental completions.
(helm-mode +1)
(setq helm-split-window-inside-p t)

(setq helm-ag-insert-at-point 'symbol)

;; Set up auto completions when editing.
(setq company-show-quick-access t)
(add-hook 'after-init-hook 'global-company-mode)

;; Set up the default app to open link in org-mode.
(setq org-file-apps
      '((auto-mode . emacs)
	("\\.pdf\\'" . "okular \"%s\"")
	("\\.xlsx\\'" . "et \"%s\"")))

;; Set up bookmark file.
(setq bookmark-default-file
      (expand-file-name "bookmarks" fcs-savefile-dir))

;; Set up project management.
(setq projectile-cache-file
      (expand-file-name "projectile.cache" fcs-savefile-dir))

(setq projectile-known-projects-file
      (expand-file-name "projectile-bookmarks.eld" fcs-savefile-dir))

;; Set up backup directory.
(setq backup-directory-alist `((".*" . ,fcs-savefile-dir)))
(setq auto-save-file-name-transforms `((".*" ,fcs-savefile-dir t)))

(provide 'fcs-ui)
;;; fcs-ui.el ends here
