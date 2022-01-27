;;; init.el --- Emacs config entry point.

;;; Commentary:

;;; Code:

;; Load the newest byte code.
(setq load-prefer-newer t)

;; Set up garbage collection.
(setq gc-cons-threshold 134217728; 128 MB
      gc-cons-percentage 0.1)

;; Set up the configration directory.
(defvar aloe-root-dir
  (file-name-directory load-file-name))

(defvar aloe-elpa-dir
  (expand-file-name "elpa" aloe-root-dir))

(defvar aloe-modules-dir
  (expand-file-name "modules" aloe-root-dir))

(defvar aloe-savefile-dir
  (expand-file-name "savefile" aloe-root-dir))

(defun aloe-make-directory (dir)
  "Create a directory DIR if not exist."
  (unless (file-exists-p dir)
    (make-directory dir)))

(aloe-make-directory aloe-elpa-dir)
(aloe-make-directory aloe-modules-dir)
(aloe-make-directory aloe-savefile-dir)

(add-to-list 'load-path aloe-modules-dir)

(setq custom-file
      (expand-file-name "custom.el" aloe-root-dir))

;; Set up backup directory.
(setq backup-directory-alist
      `((".*" . ,aloe-savefile-dir)))

(setq auto-save-file-name-transforms
      `((".*" ,aloe-savefile-dir t)))

;; Set up dependent packages.
(require 'cl-lib)
(require 'package)

(setq package-archives
      '(("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

(setq package-user-dir aloe-elpa-dir)

(package-initialize)

(defvar aloe-needed-packages '(ag beacon ccls company company-go
  clang-format doom-modeline evil flycheck go-mode go-eldoc helm
  helm-ag helm-projectile hl-todo lsp-mode neotree smartparens
  projectile which-key yasnippet yasnippet-snippets
  zenburn-theme))

(defun aloe-packages-installed-p ()
  "Check whether all needed packages are installed."
  (cl-every #'package-installed-p aloe-needed-packages))

(defun aloe-package-install (package)
  "Install PACKAGE if not installed."
  (unless (package-installed-p package)
    (package-install package)))

(defun aloe-needed-packages-prepare ()
  "Prepare all needed packages."
  (unless (aloe-packages-installed-p)
    (package-refresh-contents)
    (mapc #'aloe-package-install aloe-needed-packages)))

;; Prepare all needed packages.
(aloe-needed-packages-prepare)

;; Set up bookmark file.
(require 'bookmark)
(setq bookmark-default-file
      (expand-file-name "bookmarks" aloe-savefile-dir))

;; Set up color theme.
(add-to-list 'default-frame-alist '(background-color . "black"))
(load-theme 'zenburn t)

;; Set up line number style.
(custom-set-faces
 '(line-number ((t (:background "black" :foreground "#6F6F6F")))))

;; Hide menu bar.
(menu-bar-mode -1)

;; Highlight the `TODO' annotation.
(require 'hl-todo)
(global-hl-todo-mode +1)

;; Shine the cursor when jumping.
(require 'beacon)
(beacon-mode +1)

;; Set up the mode line style.
(require 'doom-modeline)
(doom-modeline-mode +1)

;; Show the shortcut when using a command.
(require 'which-key)
(which-key-mode +1)

;; Set up for parentheses.
(require 'smartparens-config)
(smartparens-global-mode +1)

;; Disable wrapping long lines.
(set-default 'truncate-lines t)

;; Delete trailing whitespace when saving a file.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Set up the default app to open link in org-mode.
(setq org-file-apps
      '((auto-mode . emacs)
	("\\.pdf\\'" . "okular \"%s\"")
	("\\.xlsx\\'" . "et \"%s\"")))

;; Use helm for incremental completions.
(require 'helm)
(helm-mode +1)
(setq helm-split-window-inside-p t)

(require 'helm-ag)
(setq helm-ag-insert-at-point 'symbol)

;; Set up auto completions when editing.
(require 'company)
(setq company-show-quick-access t)
(add-hook 'after-init-hook #'global-company-mode)

;; Set up project management.
(require 'projectile)
(require 'helm-projectile)

(setq projectile-cache-file
      (expand-file-name "projectile.cache" aloe-savefile-dir))

(setq projectile-known-projects-file
      (expand-file-name "projectile-bookmarks.eld" aloe-savefile-dir))

;; Set up for basic coding.
(defun aloe-basic-coding-setup-h ()
  "Basic coding mode setup hook."
  (hs-minor-mode +1)
  (projectile-mode +1)
  (helm-projectile-on)
  (display-line-numbers-mode +1))

(with-eval-after-load 'prog-mode
  (add-hook 'prog-mode-hook #'aloe-basic-coding-setup-h))

;; Set up for elisp coding.
(defun aloe-elisp-setup-h ()
  "Elisp mode setup hook."
  (flycheck-mode +1))

(with-eval-after-load 'elisp-mode
  (add-hook 'emacs-lisp-mode-hook #'aloe-elisp-setup-h))

;; Set up for c/c++ coding.
;; binary: ccls.
(require 'ccls)

(defun aloe-cc-basic-setup ()
  "C/C++ coding style."
  (setq c-basic-offset 4))

(defun aloe-cc-flycheck-setup ()
  "C/C++ syntax check setup."
  (flycheck-mode +1)
  (setq-default flycheck-disabled-checkers
                '(c/c++-clang c/c++-cppcheck c/c++-gcc)))

(defun aloe-cc-lsp-setup ()
  "C/C++ language server setup."
  (setq ccls-executable (executable-find "ccls"))
  (setq lsp-headerline-breadcrumb-enable nil)
  (lsp))

(defun aloe-clang-format-smart ()
  "Reformat buffer if .clang-format exists in the projectile root."
  (when (f-exists? (expand-file-name ".clang-format" (projectile-project-root)))
    (clang-format-buffer)))

(defun aloe-clang-format-setup ()
  "Add auto-save hook for aloe-clang-format-smart."
  (add-hook 'before-save-hook 'aloe-clang-format-smart nil t))

(defun aloe-cc-setup-h ()
  "C/C++ mode setup hook."
  (aloe-cc-basic-setup)
  (aloe-cc-flycheck-setup)
  (aloe-cc-lsp-setup)
  (aloe-clang-format-setup))

(with-eval-after-load 'cc-mode
  (add-hook 'c-mode-common-hook #'aloe-cc-setup-h))

;; Set up for golang coding.
;; binary: goimports, gofmt, gocode, godef, golint, godoc.
(require 'company-go)
(defun aloe-go-setup-h ()
  "Golang mode setup hook."
  (subword-mode +1)
  (go-eldoc-setup)
  (setq tab-width 4)
  (set (make-local-variable 'company-backends) '(company-go))
  (let ((goimports (executable-find "goimports")))
    (when goimports (setq gofmt-command goimports)))
  (add-hook 'before-save-hook #'gofmt-before-save nil t))

(with-eval-after-load 'go-mode
  (add-hook 'go-mode-hook #'aloe-go-setup-h))

;; Use shift + arrow key to switch between visible buffers.
(require 'windmove)
(windmove-default-keybindings)

(add-hook 'org-shiftup-final-hook #'windmove-up)
(add-hook 'org-shiftdown-final-hook #'windmove-down)
(add-hook 'org-shiftleft-final-hook #'windmove-left)
(add-hook 'org-shiftright-final-hook #'windmove-right)

;; Set up neotree.
(require 'evil)
(require 'neotree)

(defun neotree-evil-keys-h ()
  "Define neotree keybindings in evil-mode."
  (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
  (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
  (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "g") 'neotree-refresh)
  (evil-define-key 'normal neotree-mode-map (kbd "n") 'neotree-next-line)
  (evil-define-key 'normal neotree-mode-map (kbd "p") 'neotree-previous-line)
  (evil-define-key 'normal neotree-mode-map (kbd "A") 'neotree-stretch-toggle)
  (evil-define-key 'normal neotree-mode-map (kbd "H") 'neotree-hidden-file-toggle))

(add-hook 'neotree-mode-hook #'neotree-evil-keys-h)

(global-set-key (kbd "<f8>") 'neotree-toggle)

;; Set up vim key bindings.
(evil-mode +1)
(define-key evil-normal-state-map (kbd "M-.") 'xref-find-definitions)
(define-key evil-normal-state-map (kbd "M-,") 'xref-pop-marker-stack)
(define-key evil-normal-state-map (kbd "TAB") 'org-cycle)

;; Set up key bindings.
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c a") 'projectile-ag)
(global-set-key (kbd "C-c f") 'projectile-find-file)

;;; init.el ends here
