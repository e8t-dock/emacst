;; makes handling lisp expressions much, much easier
;; Cheatsheet: http://www.emacswiki.org/emacs/PareditCheatsheet
(use-package paredit
  :ensure t)

(use-package rainbow-delimiters
  :ensure t)

;; extra syntax highlighting for clojure
(use-package clojure-mode-extra-font-locking
  :ensure t)

;; integration with a Clojure REPL
;; https://github.com/clojure-emacs/cider
(use-package cider
  :ensure t
  :config
  ;; go right to the REPL buffer when it's finished connecting
  (setq cider-repl-pop-to-buffer-on-connect t)
  ;; When there's a cider error, show its buffer and switch to it
  (setq cider-show-error-buffer t)
  (setq cider-auto-select-error-buffer t)
  ;; Where to store the cider history.
  (setq cider-repl-history-file "~/.emacs.d/cider-history")
  ;; Wrap when navigating history.
  (setq cider-repl-wrap-history t)
  :hook ((eldoc-mode . cider-mode)
         ;; (cider-mode . clojure-mode)
         (paredit-mode . cider-repl-mode)))

;; key bindings
;; these help me out with the way I usually develop web apps
(defun cider-start-http-server ()
  (interactive)
  (cider-load-current-buffer)
  (let ((ns (cider-current-ns)))
    (cider-repl-set-ns ns)
    (cider-interactive-eval (format "(println '(def server (%s/start))) (println 'server)" ns))
    (cider-interactive-eval (format "(def server (%s/start)) (println server)" ns))))

(defun cider-refresh ()
  (interactive)
  (cider-interactive-eval (format "(user/reset)")))

(defun cider-user-ns ()
  (interactive)
  (cider-repl-set-ns "user"))

(eval-after-load 'cider
  '(progn
     (define-key clojure-mode-map (kbd "C-c C-v") 'cider-start-http-server)
     (define-key clojure-mode-map (kbd "C-M-r") 'cider-refresh)
     (define-key clojure-mode-map (kbd "C-c u") 'cider-user-ns)
     (define-key cider-mode-map (kbd "C-c u") 'cider-user-ns)))

;; key bindings and code colorization for Clojure
;; https://github.com/clojure-emacs/clojure-mode
(use-package clojure-mode
  :ensure t
  :mode
  ;; Use clojure mode for other extensions
  ("\\.edn$" . clojure-mode)
  ("\\.boot$" . clojure-mode)
  ("\\.cljs.*$" . clojure-mode)
  ("lein-env" . enh-ruby-mode)
  :config
  (defun syntax-highlighting-mode-hook ()
    (setq inferior-lisp-program "lein repl")
    (font-lock-add-keywords
     nil
     '(("(\\(facts?\\)"
        (1 font-lock-keyword-face))
       ("(\\(background?\\)"
        (1 font-lock-keyword-face))))
    (define-clojure-indent (fact 1))
    (define-clojure-indent (facts 1))
    (rainbow-delimiters-mode))
  :hook ((paredit-mode . clojure-mode)
         (subword-mode . clojure-mode)
         (syntax-highlighting-mode . clojure-mode)))

(provide 'clojure)
