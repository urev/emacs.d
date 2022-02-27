;;; fcs-keys.el --- emacs key bindings.
;;; Commentary:
;;; Code:

(require 'evil)
(require 'neotree)
(require 'windmove)

 ; Use shift + array key to switch between visible buffers.
(windmove-default-keybindings)

(add-hook 'org-shiftup-final-hook #'windmove-up)
(add-hook 'org-shiftdown-final-hook #'windmove-down)
(add-hook 'org-shiftleft-final-hook #'windmove-left)
(add-hook 'org-shiftright-final-hook #'windmove-right)

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

(provide 'fcs-keys)
;;; fcs-keys.el ends here
