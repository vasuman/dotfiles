(require 'cl-lib)

;; Packages installed -- add here whenever new package installed
(defvar my-packages '(
              cider
              haskell-mode
              wrap-region
              expand-region
              magit
              go-mode
              markdown-mode
              ))

;; Check and install packages
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(defun packages-installed-p (ps) (cl-every 'package-installed-p ps))

(require 'package)

(package-initialize)

(unless (packages-installed-p my-packages)
 (package-refresh-contents)
 (dolist (p my-packages)
  (when (not (package-installed-p p))
   (package-install p))))

;; Custom functions
(defun untabify-buffer () (interactive) (untabify (point-min) (point-max)))

;; Keybindings
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "ESC M-g") 'magit-status)
(global-set-key (kbd "ESC M-d") 'delete-trailing-whitespace)
(global-set-key (kbd "ESC M-e") 'eshell)
(global-set-key (kbd "ESC M-r") 'recompile)
(global-set-key (kbd "C-x p") 'previous-multiframe-window)

;; Preferences
(setq backup-by-copying t)
(setq backup-directory-alist '(("." . "~/.saves")))
(setq c-basic-offset 4)
(setq tab-width 4)
(setq delete-old-versions t)
(setq inhibit-startup-screen t)
(setq initial-scratch-message "")
(setq kept-new-versions 6)
(setq keyboard-coding-system 'utf-8-unix)
(setq version-control t)
(setq fill-column 80)
(setq-default indent-tabs-mode nil)
(setq-default truncate-lines t)
(wrap-region-global-mode t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(ido-mode t)
(ido-everywhere t)
(global-auto-revert-mode 1)
(set-face-attribute 'default nil :font "Terminus" :height 110)
(set-language-environment "UTF-8")

;; Haskell
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

;; Apsell
(setq ispell-program-name "aspell")
(setq ispell-personal-dictionary "~/.ispell")

;; Golang
(setq gofmt-command "goimports")
(add-hook 'before-save-hook #'gofmt-before-save)
(add-hook 'go-mode-hook (lambda ()
  (set (make-local-variable 'compile-command) "go build -o ")))

;; Markdown
(add-hook 'markdown-mode-hook #'auto-fill-mode)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Eshell
(defun eshell-restart-proc ()
  (interactive)
  (eshell-kill-process)
  (sleep-for 1) ;; Have to
  (eshell-previous-matching-input-from-input 0)
  (eshell-send-input))
(add-hook 'eshell-mode-hook
          (lambda () (define-key eshell-mode-map
                       (kbd "C-c r") 'eshell-restart-proc)))

;; Elisp
(defun load-current-file ()
  (interactive)
  (load-file (buffer-file-name (current-buffer))))
(define-key emacs-lisp-mode-map
  (kbd "C-c l") 'load-current-file)

;; Windows maximize window
(defun maximize-frame ()
  (interactive)
  (when (eq system-type 'windows-nt)
    (w32-send-sys-command 61488)))
(add-hook 'window-setup-hook 'maximize-frame t)
