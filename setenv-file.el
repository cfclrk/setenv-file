;;; setenv-file.el --- Set or unset environment variables from a file  -*- lexical-binding: t; -*-

;; Copyright (C) 2020 by Chris Clark

;; Author: Chris Clark <cfclrk@gmail.com>
;; URL: https://github.com/cfclrk/setenv-file.el
;; Keywords: convenience, environment
;; Package-Requires: ((emacs "24.1") (dash "2.17.0") (f "0.20.0") (s "1.12.0"))
;; Package-Version: 0.0.1

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Set or unset environment variables from a file.
;;
;; This package provides an interactive function `setenv-file` to set
;; environment variables defined in a file. With one `C-u` prefix argument,
;; `setenv-file` unsets the environment variables defined in the file.
;;
;; When used interactively, `setenv-file` prompts for a file, defaulting to the
;; directory `setenv-file-dir`.
;;
;;
;; # Usage
;;
;; Load `setenv-file.el` in Emacs, and optionally set a `setenv-file-dir`:
;;
;;     (load "/path/to/setenv-file.el")
;;     (setq setenv-file-dir (expand-file-name "~/.env/"))
;;
;; Create a file in `setenv-file-dir` with environment variable definitions. For
;; example:
;;
;;     FOO=~/foo
;;     BAR=$FOO/bar
;;     ОФИС=ДОМ
;;
;; And now you can set those environment variables in Emacs using `M-x
;; setenv-file`, and navigate to the file. View your new environment variables
;; with `M-x getenv`.
;;
;;
;; # File Format
;;
;; Each line in the file should be in a `KEY=VALUE` format, with one entry per
;; line. This package does not invoke a shell to interpret the file, so most
;; shell-isms will not work. However, the env file may:
;;
;; -   Use existing environment variables
;; -   Define an environment variable and use it in successive lines
;; -   A `~` is expanded if it is the first character in the value
;;
;;; Code:

(require 'dash)
(require 'f)
(require 's)

;;; Options

(defgroup setenv-file nil
  "Source environment variable files in Emacs."
  :group 'environment
  :prefix "setenv-file-"
  :link '(url-link :tag "GitHub" "https://github.com/cfclrk/setenv-file"))

(defcustom setenv-file-dir (expand-file-name "~/")
  "Directory with env files."
  :group 'setenv-file
  :type 'file)

;;; Public

(defun setenv-file (f)
  "Set or unset environment variables from file F.
When used interactively, `setenv-file' prompts for the file
to load, defaulting to the directory `source-env-dir'.

The env file F may make use of existing environment variables,
and tildes are expanded if they are the first character of the
value. However, other shell-isms will not work.

Prefixed with one \\[universal-argument], unset the environment
variables defined in file F."
  (interactive (list (read-file-name "ENV file: " setenv-file-dir)))
  (let* ((lines (s-lines (s-trim (f-read-text f))))
         (pairs (--map (s-split "=" it) lines)))
    (if current-prefix-arg
        (setenv-file-unset-pairs pairs)
      (setenv-file-export-pairs pairs))))

(defun setenv-file-export-pairs (pairs)
  "Add PAIRS to `process-environment'.
PAIRS is a list of pairs, where each pair is an environment
variable name and value."
  (-each pairs #'setenv-file--export-pair))

(defun setenv-file-unset-pairs (pairs)
  "Remove PAIRS from `process-environment'.
PAIRS is a list of pairs, where each pair is an environment
variable name and value. The value is discarded; the environment
variable will be removed regardless of its value."
  (setenv-file--unset-names (-map 'car pairs)))

;;; Private

(defun setenv-file--export-pair (pair)
  "Set an environment variable PAIR.
PAIR is a list of size 2, where first element is an environment
variable name and the second element is the value.

If the second element begins with a ~, it is treated as a file
path and expanded."
  (let ((name (car pair))
        (val (car (cdr pair))))
    (if (string-prefix-p "~" val)
        (setenv name (expand-file-name val) t)
      (setenv name val t))))

(defun setenv-file--unset-names (names)
  "Remove NAMES from `process-environment'.
NAMES is a list of environment variable names which may or may
not be currently set. This function removes each given name from
`process-environment' if it is set."
  (-each names #'setenv-file--unset-name))

(defun setenv-file--unset-name (name)
  "Unset the environment variable NAME.
Unset environment variable NAME by removing it from
`process-environment' if it is there.

Note: calling `setenv' with a prefix argument sets the variable's
value to nil, but the variable is still present. This function
completely removes the variable from `process-environment'."
  (let* ((name (if (multibyte-string-p name)
                   (encode-coding-string name locale-coding-system t)
                 name))
         (index (-elem-index name (setenv-file--get-names))))
    (if index
        (setq process-environment (-remove-at index process-environment))
      process-environment)))

(defun setenv-file--get-names ()
  "Return names of all current environment variables."
  (--map (car (s-split "=" it)) process-environment))

(provide 'setenv-file)
;;; setenv-file.el ends here
