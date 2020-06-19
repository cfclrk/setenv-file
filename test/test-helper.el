;;; test-helper.el --- Helpers for source-env-file-test.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; Ert-runner evaluates this file before running tests.

;;; Code:

(require 'cask)
(require 'dash)
(require 'f)

(defvar setenv-file/root-dir (f-dirname (f-dirname (f-this-file))))
(defvar setenv-file/test-dir (f-dirname (f-this-file)))

;; Require all Elisp files in this package
(let* ((cask-bundle (cask-setup setenv-file/root-dir))
       (proj-files (cask-files cask-bundle))
       (el-files (--filter (f-ext? it "el") proj-files))
       (abs-el-files (--map (f-join setenv-file/root-dir it) el-files)))
  (--each abs-el-files
    (require (intern (f-base it)) it)))

;;; Test fixtures
;;; -------------

(defmacro with-process-environment (temp-environment &rest forms)
  "Set `process-environment' to TEMP-ENVIRONMENT and evaluate FORMS.
After evaluating FORMS, the original `process-environment' is restored."
  `(let ((orig-process-environment process-environment))
    (unwind-protect
        (progn
          (setq process-environment ,temp-environment)
          ,@forms)
      (setq process-environment orig-process-environment))))

;;; test-helper.el ends here
