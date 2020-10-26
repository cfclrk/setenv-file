;;; setenv-file-test.el --- Tests for package setenv-file  -*- lexical-binding: t;

;;; Commentary:

;; Tests for package setenv-file.

;;; Code:

(ert-deftest setenv-file/simple ()
  "Test running `setenv-file' to set env vars."
  (with-process-environment '()
    (let ((test-file (proj-file "test/examples/variables")))
      (setenv-file test-file)
      (should (equal "foo" (getenv "FOO")))
      (should (equal "foo-bar" (getenv "BAR")))
      (should (equal (expand-file-name "~/cats") (getenv "BAZ"))))))

(ert-deftest setenv-file/with-prefix-arg ()
  "Test running `setenv-file' with a prefix arg to unset simple
env vars."
  (with-process-environment '("FOO=foo" "BAR=bar")
    (let ((test-file (proj-file "test/examples/simple"))
          (current-prefix-arg 4))
      (setenv-file test-file)
      (should (equal nil (getenv "FOO")))
      (should (equal nil (getenv "BAR")))
      (should (equal '() process-environment)))))

(ert-deftest setenv-file/multibyte ()
  "Test running `setenv-file' to set env vars with multibyte characters."
  (with-process-environment '()
    (should (equal nil (getenv "A")))
    (let ((test-file (proj-file "test/examples/multibyte")))
      (setenv-file test-file)
      (should (equal "Д" (getenv "Ф")))
      (should (equal "µ" (getenv "¥")))
      (should (equal '("\302\245=\302\265" "\320\244=\320\224")
                     process-environment)))))

(ert-deftest setenv-file/multibyte-with-prefix-arg ()
  "Test running `setenv-file' with a prefix arg to unset
multibyte env vars."
  (with-process-environment
      '("\302\245=\302\265" "\320\244=\320\224")
    (should (equal "Д" (getenv "Ф")))
    (should (equal "µ" (getenv "¥")))
    (let ((test-file (proj-file "test/examples/multibyte"))
          (current-prefix-arg 4))
      (setenv-file test-file)
      (should (equal nil (getenv "Ф")))
      (should (equal nil (getenv "¥"))))))

(ert-deftest setenv-file-export-pairs ()
  "Test running `setenv-file-export-pairs' to set env vars."
  (with-process-environment '()
    (setenv-file-export-pairs '(("A" "a")
                                ("B" "b")))
    (should (equal "a" (getenv "A")))
    (should (equal "b" (getenv "B")))
    (should (equal '("B=b" "A=a") process-environment))))

(ert-deftest setenv-file-unset-pairs ()
  "Test running `setenv-file-unset-pairs' to unset env vars."
  (with-process-environment '("FOO=foo" "BAR=bar" "BAZ=baz")
    (setenv-file-unset-pairs '(("FOO" "foo") ("BAR" "barrr")))
    (should (equal nil (getenv "FOO")))
    (should (equal nil (getenv "BAR")))
    (should (equal "baz" (getenv "BAZ")))))

(ert-deftest setenv-file--unset-names/simple ()
  "Test running `setenv-file-unset-names' to unset env vars."
  (with-process-environment '("FOO=foo" "BAR=bar" "BAZ=baz")
    (setenv-file--unset-names '("FOO" "BAR"))
    (should (equal nil (getenv "FOO")))
    (should (equal nil (getenv "BAR")))
    (should (equal "baz" (getenv "BAZ")))
    (should (equal '("BAZ=baz") process-environment))))

(ert-deftest setenv-file--unset-names/non-existent-name ()
  "Test running `setenv-file--unset-names' to unset non-existent env var.
This shouldn't cause a problem. The environment should remain
unchanged."
  (with-process-environment '("FOO=foo")
    (setenv-file--unset-names '(("BAR" "bar")))
    (should (equal nil (getenv "BAR")))
    (should (equal '("FOO=foo") process-environment))))

(ert-deftest setenv-file--export-pair ()
  "Test running `setenv-file--export-pair' to set a single
environment variable."
  (with-process-environment '()
    (setenv-file--export-pair '("FOO" "foo"))
    (should (equal "foo" (getenv "FOO")))))

(ert-deftest setenv-file--unset-name/simple ()
  "Test running `setenv-file--unset-name' to unset a single,
simple environment variable."
  (with-process-environment '("CATS=cats")
    (should (equal "cats" (getenv "CATS")))
    (setenv-file--unset-name "CATS")
    (should (equal nil (getenv "CATS")))))

(ert-deftest setenv-file--unset-name/multibyte ()
  "Test running `setenv-file--unset-name' to unset a single,
multibyte environment variable."
  (with-process-environment '("\320\224=\320\244")
    (should (equal "Ф" (getenv "Д")))
    (setenv-file--unset-name "Д")
    (should (equal nil (getenv "Д")))))

;;; setenv-file-test.el ends here
