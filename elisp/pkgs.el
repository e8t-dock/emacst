(use-package color-moccur
             :command (isearch-moccur isearch-all)
             :bind (("M-s O" . moccur)
                    :map isearch-mode-map
                    ("M-o" . isearch-moccur)
                    ("M-O" . isearch-moccur-all))
             :init
             (setq isearch-lazy-highlight t)
             :config
             (use-package moccur-edit))

(provide 'init-pkgs)
