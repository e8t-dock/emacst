; https://blog.bitsandbobs.net/blog/emacs-markdown-live-preview/

(use-package markdown-mode
  :ensure t
  ; :mode "\\.rb\\'"
  :mode ("\\.md\\'" . gfm-mode)
  :commands (markdown-mode gfm-mode)
  :config
  ; 需要安装 pandoc
  ; https://pandoc.org/installing.html
  (setq markdown-command "pandoc -t html5"))

; Preview
(use-package simple-httpd 
  :ensure t
  :config
  (setq httpd-port 7070)
  ; (setq httpd-host (system-name)))
  (setq httpd-host "localhost"))

(use-package impatient-mode
   :ensure t
   :commands impatient-mode)

(defun tiny-markdown-filter (buffer)
  (princ
    (with-temp-buffer
      (let ((tmp (buffer-name)))
        (set-buffer buffer)
        (set-buffer (markdown tmp))
        (format "<!DOCTYPE html><html><title>Markdown preview</title><link rel=\"stylesheet\" href = \"https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/3.0.1/github-markdown.min.css\"/><body><article class=\"markdown-body\" style=\"box-sizing: border-box;min-width: 200px;max-width: 980px;margin: 0 auto;padding: 45px;\">%s</article></body></html>" (buffer-string))))
    (current-buffer)))

(defun tiny-markdown-preview ()
  "Preview Markdown"
  (interactive)
  (unless (process-status "httpd")
    (httpd-start))
  (impatient-mode)
  (imp-set-user-filter 'tiny-markdown-filter)
  (imp-visit-buffer))

(provide 'markdown)
