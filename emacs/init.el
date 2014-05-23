(require 'cl-lib)

;; Check and install packages
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/") 
                         ("marmalade" . "http://marmalade-repo.org/packages/") 
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(defvar my-packages '(cider haskell-mode wrap-region expand-region magit))
(defun packages-installed-p (ps) (cl-every 'package-installed-p ps))

(require 'package)

(package-initialize)

(unless (packages-installed-p my-packages)
  (package-refresh-contents)
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p))))

;; Keybindings
(global-set-key (kbd "C-=") 'er/expand-region)

;; Hooks
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

;; Settings
(setq backup-by-copying t)
(setq backup-directory-alist '(("." . "~/.saves")))
(setq c-basic-offset 4)
(setq custom-enabled-themes '(whiteboard))
(setq delete-old-versions t)
(setq inhibit-startup-screen t)
(setq initial-scratch-message "")
(setq kept-new-versions 6)
(setq keyboard-coding-system 'utf-8-unix)
(setq version-control t)
(wrap-region-global-mode t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(ido-mode t)
(ido-everywhere t)
(windmove-default-keybindings 'meta)
(global-auto-revert-mode 1)

(setq fill-column 80)
(setq-default truncate-lines t)
