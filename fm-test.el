;;; fm-test.el --- Tests for fm.el
;;; -*- lexical-binding: t; -*-
(require 'buttercup)
(require 'fm)

(describe "fm--run-fsel"
  (it "returns success status and output"
    (let ((fm-fsel-executable "echo"))
      (expect (fm--run-fsel nil "-l")
              :to-equal '(0 . "-l\n"))))

  (it "handles command with arguments"
    (let ((fm-fsel-executable "echo"))
      (expect (fm--run-fsel nil "test" "test2")
              :to-equal '(0 . "test test2\n"))))

  (it "detects incorrect arguments"
    (let ((fm-fsel-executable "fsel"))
      (let ((result (fm--run-fsel nil "-W" "-Z")))
        (expect (car result) :not :to-equal 0)
        (expect (cdr result) :to-match ".*")))))

(describe "fm-selections"
  (it "returns success message"
    (let ((fm-fsel-executable "fsel"))
      (expect (fm-selection-append '("/etc/fstab" "test2"))
              :to-equal "Paths added to selection"))))

;;(buttercup-run-tests-in-buffer)

;;; fm-test.el ends here
