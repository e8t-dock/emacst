(setq user-init-file (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (file-name-directory user-init-file))

;; Global

(defconst *is-a-mac* (eq system-type 'darwin))

;; Debug

;; (setq debug-on-error t)

;; Load config files

;; (add-to-list 'load-path (expand-file-name "custom" user-emacs-directory))
;; (require 'init')

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load-file custom-file)

;; Config elpa
(setq package-enable-at-startup nil)
(require 'package)
(add-to-list 'package-archives '("gnu-ec" . "http://elpa.emacs-china.org/gnu/") t)
(add-to-list 'package-archives '("melpa-ec" . "http://elpa.emacs-china.org/melpa/") t)
(package-initialize)

;; packages list 
;; In Elips any function ending with a -p is a predicate.

;; (defvar my-packages '(dracula-theme company))
;; (dolist (p my-packages)
;; 	(when (not (package-installed-p p))
;; 		(package-install p)))


;; Load theme 

(load-theme 'dracula)

;; (use-package dracula-theme
;; 						 :config
;; 						 (load-theme 'dracula))

;; Config packages 

;; check if use-package is installed 

(when (not (package-installed-p 'use-package))
	(package-refresh-contents)
	(package-install 'use-package))

;; company 

;; (global-company-mode t)
;; 
;; ;; navigate in completion minibuffer
;; (define-key company-active-map (kbd "C-n") 'company-select-next)
;; (define-key company-active-map (kbd "C-p") 'company-select-previous)
;; 
;; (setq company-idle-delay 0.0)

(use-package company
						 :bind (:map company-active-map
												 ("C-n" . company-select-next)
												 ("C-p" . company-select-previous))
						 :config
						 (setq company-idle-delay 0.3)
						 (global-company-mode t))

(use-package magit
						 ;; auto download missing package
						 :ensure t
						 :bind ("C-x g" .  magit-status))

(use-package lsp-mode
    :ensure t
    :init
    (setq lsp-keymap-prefix "s-l")
    :hook
    ((go-mode .lsp-deferred))
    :commands (lsp lsp-deferred)
    :config
    ;; https://emacs-lsp.github.io/lsp-mode/page/performance/
    (setq gc-cons-threshold 100000000)
    (setq read-process-output-max (* 1024 1024)))

  ;; I'm not sure what this does yet. I forgot I'd installed it.
(use-package lsp-ui :commands lsp-ui-mode)

(use-package helm-lsp
  :ensure t
  :commands helm-lsp-workspace-symbol)

(use-package go-mode
  :ensure t
  :config
  ;; To consider:
  ;; (add-hook 'before-save-hook #'gofmt-before-save)
  )

(use-package exec-path-from-shell
  :ensure t)
(if *is-a-mac*
  (add-hook 'after-init-hook 'exec-path-from-shell-initialize))

;; Config UI

(when (member "Menlo" (font-family-list))
  (set-frame-font "Menlo-16" t t))

(setq default-frame-alist
      '((height . 35) (width . 85) (top . 20) (left . 20) (menu-bar-lines . 20) (tool-bar-lines . 0)))

(global-linum-mode)

;; Config GUI

(setq inhibit-startup-message t)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(global-hl-line-mode t)

;; Use `command` as `meta` in macOS
;; (setq mac-command-modifier 'meta')

;; Key Binding

(global-set-key "\C-ca" 'org-agenda)

;; custom function

(defun revert-buffer-yes ()
	"Revert buffer without confirmation."
	(interactive)
	(revert-buffer :ignore-auto :noconfirm))
