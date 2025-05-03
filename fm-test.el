;;; fm-test.el --- Tests for fm.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'fm)

(ert-deftest fm--run-fsel-success ()
  "Test successful execution of a command."
  (let ((fm-fsel-executable "fsel"))
    (should (equal (fm--run-fsel "list")
                   (cons 0 "list\n")))))

(ert-deftest fm--run-fsel-failure ()
  "Test failure when command is invalid."
  (let ((fm-fsel-executable "invalid-command"))
    (should-error (fm--run-fsel "list"))))

(ert-deftest fm--run-fsel-with-args ()
  "Test command with arguments."
  (let ((fm-fsel-executable "fsel"))
    (should (equal (fm--run-fsel "add" "test.txt")
                   (cons 0 "add test.txt\n")))))

(ert-deftest fm--run-fsel-timeout ()
  "Test timeout handling (simulated)."
  (let ((fm-fsel-executable "sleep"))
    (should (equal (fm--run-fsel "list")
                   (cons 0 "list\n")))))

(ert-deftest fm--run-fsel-incorrect-args ()
  "Test incorrect arguments."
  (let ((fm-fsel-executable "fsel"))
    (let ((result (fm--run-fsel "invalid-command" "arg")))
      (should (not (zerop (car result)))
              (should (string-match ".*invalid command.*" (cdr result)))))))
