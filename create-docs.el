;;; create-docs.el --- Create package docs  -*- lexical-binding: t; -*-

;;; Commentary:

;; Create a README.md and texinfo documentation from the package org file.

;;; Code:

(require 'ox-gfm)
(require 'ox-texinfo)
(require 'markdown-mode)

(defun create-docs ()
  "Export setenv-file.org to README.md and setenv-file.info."
  (let ((make-backup-files nil))

    ;; Create README.md
    (with-temp-buffer
      (insert-file-contents "./setenv-file.org")
      (org-export-to-file 'gfm "./README.md"))

    ;; Format README.md
    (with-temp-buffer
      (insert-file-contents "./README.md")
      (markdown-mode)
      (fill-region (point-min) (point-max))
      (write-file "./README.md"))

    ;; Create texinfo docs
    (with-temp-buffer
      (insert-file-contents "./setenv-file.org")
      (org-export-to-file 'texinfo "./setenv-file.texi"
        nil nil nil nil nil (lambda (file) (org-texinfo-compile file))))))

(provide 'create-docs)
;;; create-docs.el ends here
