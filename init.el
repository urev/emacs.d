;;; init.el --- emacs config entry point.
;;; Commentary:
;;; Code:

;; Load the newest byte code.
(setq load-prefer-newer t)

;; Set up garbage collection.
(setq gc-cons-threshold 134217728 ; 128 MB
      gc-cons-percentage 0.1)

;; Set up the configration directory.
(defconst fcs-root-dir (file-name-directory load-file-name))
(defconst fcs-elpa-dir (expand-file-name "elpa" fcs-root-dir))
(defconst fcs-modules-dir (expand-file-name "modules" fcs-root-dir))
(defconst fcs-savefile-dir (expand-file-name "savefile" fcs-root-dir))

(defun fcs-make-directory (dir)
  "Create a directory DIR if not exist."
  (unless (file-exists-p dir)
    (make-directory dir)))

(fcs-make-directory fcs-elpa-dir)
(fcs-make-directory fcs-modules-dir)
(fcs-make-directory fcs-savefile-dir)

(add-to-list 'load-path fcs-modules-dir)

(setq custom-file (expand-file-name "custom.el" fcs-root-dir))

;; Set up other configs in modules directory.
(require 'fcs-package)
(require 'fcs-ui)
(require 'fcs-keys)
(require 'fcs-cc)
(require 'fcs-elisp)
(require 'fcs-go)
(require 'fcs-python)

;;; init.el ends here
