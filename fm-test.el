;;; fm-test.el --- Tests for fm.el -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'fm)

(describe "fm--run-fsel"
  (it "returns success status and output"
    (let ((fm-fsel-executable "fsel"))
      (expect (fm--run-fsel "list")
        :to-equal '(0 . "list\n"))))

  (it "raises error on invalid command"
    (let ((fm-fsel-executable "invalid-command"))
      (expect (fm--run-fsel "list")
        :to-raise 'error)))

  (it "handles command with arguments"
    (let ((fm-fsel-executable "fsel"))
      (expect (fm--run-fsel "add" "test.txt")
        :to-equal '(0 . "add test.txt\n"))))

  (it "handles timeout (simulated)"
    (let ((fm-fsel-executable "sleep"))
      (expect (fm--run-fsel "list")
        :to-equal '(0 . "list\n"))))

  (it "detects incorrect arguments"
    (let ((fm-fsel-executable "fsel"))
      (let ((result (fm--run-fsel "invalid-command" "arg")))
        (expect (car result) :to-not-equal 0)
        (expect (cdr result) :to-match ".*invalid command.*")))))

(buttercup-run-tests-in-buffer)
