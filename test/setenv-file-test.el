;;; setenv-file-test.el --- Tests for package setenv-file  -*- lexical-binding: t;

;;; Commentary:

;; Tests for package setenv-file.

;;; Code:

(ert-deftest setenv-file/variables ()
  "Test setting environment variables from examples/variables."
  (with-process-environment
   '()
   (let ((test-file (f-join setenv-file/test-dir "examples/variables")))
     (setenv-file test-file)
     (should (equal "foo" (getenv "FOO")))
     (should (equal "foo-bar" (getenv "BAR")))
     (should (equal (expand-file-name "~/cats") (getenv "BAZ"))))))

(ert-deftest setenv-file/uneset-simple ()
  (with-process-environment
   '("FOO=foo" "BAR=bar")
   (let ((test-file (f-join setenv-file/test-dir "examples/simple"))
         (current-prefix-arg 4))
     (setenv-file test-file)
     (should (equal nil (getenv "FOO")))
     (should (equal nil (getenv "BAR")))
     (should (equal '() process-environment)))))

(ert-deftest setenv-file/multibyte ()
  "Test setting multibyte environment variables from examples/multibyte."
  (with-process-environment
   '()
   (should (equal nil (getenv "A")))
   (let ((test-file (f-join setenv-file/test-dir "examples/multibyte")))
     (setenv-file test-file)
     (should (equal "Д" (getenv "Ф")))
     (should (equal "µ" (getenv "¥")))
     (should (equal '("\302\245=\302\265" "\320\244=\320\224")
                    process-environment)))))


(ert-deftest setenv-file/multibyte-prefix ()
  "Test running `setenv-file-export' with a prefix argument.
The variables defined in the file should be unset if they exist."
  (with-process-environment
   '("\302\245=\302\265" "\320\244=\320\224")
   (should (equal "Д" (getenv "Ф")))
   (should (equal "µ" (getenv "¥")))
   (let ((test-file (f-join setenv-file/test-dir "examples/multibyte"))
         (current-prefix-arg 4))
     (setenv-file test-file)
     (should (equal nil (getenv "Ф")))
     (should (equal nil (getenv "¥"))))))

(ert-deftest export ()
  "Test setting environment variables using `setenv-file-export'."
  (with-process-environment
   '()
   (setenv-file-export '(("A" "a")
                         ("B" "b")))
   (should (equal "a" (getenv "A")))
   (should (equal "b" (getenv "B")))
   (should (equal '("B=b" "A=a") process-environment))))

(ert-deftest unset ()
  "Test unsetting environment variables using `setenv-file-unset'."
  (with-process-environment
   '("FOO=foo" "BAR=bar" "BAZ=baz")
   (setenv-file-unset '(("FOO" "foo")
                        ("BAR" "bar")))
   (should (equal nil (getenv "FOO")))
   (should (equal nil (getenv "BAR")))
   (should (equal "baz" (getenv "BAZ")))
   (should (equal '("BAZ=baz") process-environment))))

(ert-deftest unset-non-existent-name ()
  "Test unsetting an env var that doesn't exist.
Shouldn't cause a problem. The environment should remain
unchanged."
  (with-process-environment
   '("FOO=foo")
   (setenv-file-unset '(("BAR" "bar")))
   (should (equal nil (getenv "BAR")))
   (should (equal '("FOO=foo") process-environment))))

(ert-deftest export-pair ()
  "Test setting a single environment variable."
  (with-process-environment
   '()
   (setenv-file--export-pair '("FOO" "foo"))
   (should (equal "foo" (getenv "FOO")))))

(ert-deftest unset-name ()
  "Test unsetting a single environment variable."
  (with-process-environment
   '("CATS=cats")
   (should (equal "cats" (getenv "CATS")))
   (setenv-file--unset-name "CATS")
   (should (equal nil (getenv "CATS")))))

(ert-deftest unset-name-multibyte ()
  "Test unsetting a single, multibyte environment variable."
  (with-process-environment
   '("\320\224=\320\244")
   (should (equal "Ф" (getenv "Д")))
   (setenv-file--unset-name "Д")
   (should (equal nil (getenv "Д")))))

;;; setenv-file-test.el ends here
