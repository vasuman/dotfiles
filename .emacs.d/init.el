;; install packages
(require 'cl-lib)

(defvar my-packages '(
                      wrap-region
                      expand-region
                      magit
                      go-mode
                      markdown-mode
                      js2-mode
                      web-mode
                      ))

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

;; custom functions
(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun insert-timestamp ()
  (interactive)
  (insert
   (with-temp-buffer
     (shell-command
      "date --iso-8601=seconds"
      (current-buffer))
     (buffer-string))))

;; keybindings
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "ESC M-g") 'magit-status)
(global-set-key (kbd "ESC M-d") 'delete-trailing-whitespace)
(global-set-key (kbd "ESC M-e") 'eshell)
(global-set-key (kbd "ESC M-b") 'bookmark-bmenu-list)
(global-set-key (kbd "C-x p") 'previous-multiframe-window)
(global-set-key (kbd "C-<next>") 'next-multiframe-window)
(global-set-key (kbd "C-<prior>") 'previous-multiframe-window)

;; preferences
(set-face-attribute 'default nil :font "Anonymous Pro")

(set-language-environment "UTF-8")
(setq keyboard-coding-system 'utf-8-unix)

(setq backup-by-copying t)
(setq backup-directory-alist '(("." . "~/.saves")))
(setq delete-old-versions t)
(setq kept-new-versions 6)
(setq version-control t)

(setq tab-width 4)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)
(setq-default indent-tabs-mode nil)

(setq fill-column 80)
(column-number-mode)
(setq-default truncate-lines t)

(setq visible-bell 1)

(menu-bar-mode -1)
(tool-bar-mode -1)
(ido-mode t)
(ido-everywhere t)
(global-auto-revert-mode 1)
(wrap-region-global-mode t)
(show-paren-mode 1)

(windmove-default-keybindings 'meta)

;; start maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Dired
(eval-after-load 'dired
  (lambda ()
    (define-key dired-mode-map
      (kbd "/")
      'dired-isearch-filenames)
    (define-key dired-mode-map
      (kbd "^")
      (lambda ()
        (interactive)
        (find-alternate-file "..")))))

;; spelling
(setq ispell-program-name "aspell")
(setq ispell-personal-dictionary "~/.ispell")

;; Golang
(defun go-setup-hook ()
  (setq compile-command "go build -v ")                          
  (add-hook 'before-save-hook #'gofmt-before-save)
  (define-key go-mode-map
    (kbd "C-c C-d") 'godoc)
  (define-key go-mode-map
    (kbd "C-c C-.") 'godoc-at-point)
  (define-key go-mode-map
    (kbd "C-.") 'godef-jump))

(add-hook 'go-mode-hook 'go-setup-hook)
(setq gofmt-command "goimports")

(eval-after-load 'go-mode
  `(load-file "$GOPATH/src/golang.org/x/tools/cmd/oracle/oracle.el"))


;; Markdown
(add-to-list 'auto-mode-alist '("\\.mkd\\'" . markdown-mode))
(add-hook 'markdown-mode-hook #'auto-fill-mode)

;; Eshell
(defun eshell-restart-proc ()
  (interactive)
  (eshell-kill-process)
  (sleep-for 1) ;; have to
  (eshell-previous-matching-input-from-input 0)
  (eshell-send-input))

(add-hook 'eshell-mode-hook
          (lambda ()
            (define-key eshell-mode-map
              (kbd "C-c r") 'eshell-restart-proc)))

;; Elisp
(defun load-current-file ()
  (interactive)
  (load-file (buffer-file-name (current-buffer))))

(define-key emacs-lisp-mode-map
  (kbd "C-c l") 'load-current-file)

;; Javascript
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(setq js2-basic-offset 2)

;; Web templates
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.tmpl\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(setq web-mode-markup-indent-offset 2)

;; startup
(setq inhibit-startup-screen t)
(setq initial-scratch-message "")

(defun custom-startup ()
  (if (string= (buffer-name (current-buffer)) "*scratch*")
      (progn (call-interactively 'bookmark-bmenu-list))))

(add-hook 'emacs-startup-hook 'custom-startup)

;; end
(put 'dired-find-alternate-file 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
