;;; setenv-file.el --- Set or unset environment variables from a file  -*- lexical-binding: t; -*-

;; Author: Chris Clark <cfclrk@gmail.com>
;; Version: 0.0.1
;; Keywords: environment
;; URL: https://github.com/cfclrk/setenv-file.el

;;; Commentary:
;;
;; This package provides an interactive function `setenv-file' to set
;; environment variables defined in a file. With one \\[universal-argument]
;; prefix argument, `setenv-file' unsets the environment variables defined in
;; the file.
;;
;; When used interactively, `setenv-file' prompts for a file, defaulting to the
;; directory `setenv-file-dir'.
;;
;; How To
;; ------
;;
;; Create a file with environment variable definitions. For example:
;;
;;     FOO=~/foo
;;     BAR=$FOO/bar
;;     ОФИС=ДОМ
;;
;; Set those environment variables in Emacs using `setenv-file', and navigate to
;; the file. View your new environment variables with `getenv'.
;;
;; To customize the default directory where you keep your env files:
;;
;;     (setq setenv-file-dir (expand-file-name "~/.env/"))
;;
;; Env File Format
;; ---------------
;;
;; Each line in the file should be in a `KEY=VALUE` format, with one entry per
;; line. This package does not invoke a shell to interpret the file, so most
;; shell-isms will not work. However, as a convenience, the env file may:
;;
;;   - Define simple shell-like variables
;;   - Use existing environment variables
;;   - Tildes are expanded if they are the first character of the value
;;
;;; Code:

(require 'dash)
(require 'f)
(require 's)

;; Customization
;; ----------------------------------------------------------------------------

(defgroup setenv-file nil
  "Source environment variable files in Emacs."
  :group 'environment
  :prefix "setenv-file-"
  :link '(url-link :tag "GitHub" "https://github.com/cfclrk/setenv-file"))

(defcustom setenv-file-dir (expand-file-name "~/")
  "Directory with env files."
  :group 'setenv-file
  :type 'file)

;; Public
;; ----------------------------------------------------------------------------

(defun setenv-file (f)
  "Set or unset environment variables from file F.
When used interactively, `setenv-file' prompts for the file
to load, defaulting to the directory `source-env-dir'.

As a convenience the env file F may define simple bash-like
variables, use existing environment variables, and tildes are
expanded if they are the first character of the value. However,
other shell-isms will not work.

Prefixed with one \\[universal-argument], unset the environment
variables defined in file F."
  (interactive (list (read-file-name "ENV file: " setenv-file-dir)))
  (let* ((lines (s-lines (s-trim (f-read-text f))))
         (pairs (--map (s-split "=" it) lines)))
    (if current-prefix-arg
        (setenv-file-unset pairs)
      (setenv-file-export pairs))))

(defun setenv-file-export (pairs)
  "Add PAIRS to `process-environment'.
PAIRS is a list of pairs, where each pair is an environment
variable name and value."
  (-each pairs 'setenv-file--export-pair))

(defun setenv-file-unset (pairs)
  "Remove PAIRS from `process-environment'.
PAIRS is a list of pairs, where each pair is an environment
variable name and value. The second element of each pair is
discarded."
  (--each pairs (setenv-file--unset-name (car it))))

;; Private
;; ----------------------------------------------------------------------------

(defun setenv-file--export-pair (pair)
  "Set or unset an environment variable.
PAIR is a list of size 2, where first element is an environment
variable name and the second element is the value.

If the second element (the value) begins with a ~, treat it as a
file path and expand it."
  (let ((name (car pair))
        (val (car (cdr pair))))
    (if (string-prefix-p "~" val)
        (setenv name (expand-file-name val) t)
      (setenv name val t))))

(defun setenv-file--unset-name (name)
  "Unset environment variable NAME.
Unset NAME by removing it from `process-environment'. While
`setenv' claims to be able to unset environment variables, it
really just sets the value of the variable to nil, so the
variable still shows up when calling `getenv'."
  (let* ((name (if (multibyte-string-p name)
                   (encode-coding-string name locale-coding-system t)
                 name))
         (index (-elem-index name (setenv-file--get-names))))
    (if index
        (setq process-environment (-remove-at index process-environment))
      process-environment)))

(defun setenv-file--get-names ()
  "Return the list of the names in `process-environment'."
  (--map (car (s-split "=" it)) process-environment))

(provide 'setenv-file)

;;; setenv-file.el ends here
