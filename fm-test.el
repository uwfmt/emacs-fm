;;; fm-test.el --- Tests for fm.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Alexander Grafov
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

(require 'buttercup)
(require 'fm)

(defvar fm-test-data-dir
  (expand-file-name "test-data" (file-name-directory (or load-file-name buffer-file-name)))
  "Directory containing test data files.")

(defvar fm-test-files
  (list (expand-file-name "test1" fm-test-data-dir)
        (expand-file-name "test2" fm-test-data-dir)
        (expand-file-name "test3" fm-test-data-dir))
  "List of test files in test-data directory.")

(defvar fm-test-folder-files
  (list (expand-file-name "folder/test1" fm-test-data-dir)
        (expand-file-name "folder/test2" fm-test-data-dir)
        (expand-file-name "folder/test3" fm-test-data-dir))
  "List of test files in test-data/folder directory.")

(describe "fm--run-fsel basic execution"
  
  (before-each
    (when (get-buffer fm-fsel-output-buffer)
      (kill-buffer fm-fsel-output-buffer))
    (when (get-buffer fm-fsel-error-buffer)
      (kill-buffer fm-fsel-error-buffer)))
  
  (it "returns zero exit status on success"
    (let ((fm-fsel-executable "echo"))
      (expect (fm--run-fsel nil nil "test") :to-equal 0)))
  
  (it "writes output to default buffer"
    (let ((fm-fsel-executable "echo"))
      (fm--run-fsel nil nil "hello" "world")
      (with-current-buffer fm-fsel-output-buffer
        (expect (string-trim (buffer-string)) :to-equal "hello world"))))
  
  (it "writes output to custom buffer when specified"
    (let ((fm-fsel-executable "echo")
          (custom-buf "*test-output*"))
      (fm--run-fsel custom-buf nil "custom" "test")
      (with-current-buffer custom-buf
        (expect (string-trim (buffer-string)) :to-equal "custom test"))
      (kill-buffer custom-buf)))
  
  (it "passes multiple arguments correctly"
    (let ((fm-fsel-executable "echo"))
      (fm--run-fsel nil nil "-r" "/path/file1" "/path/file2")
      (with-current-buffer fm-fsel-output-buffer
        (expect (string-trim (buffer-string)) 
                :to-equal "-r /path/file1 /path/file2"))))
  
  (it "sets output buffer as read-only after execution"
    (let ((fm-fsel-executable "echo"))
      (fm--run-fsel nil nil "test")
      (with-current-buffer fm-fsel-output-buffer
        (expect buffer-read-only :to-be-truthy))))
  
  (it "clears output buffer before execution"
    (let ((fm-fsel-executable "echo"))
      (with-current-buffer (get-buffer-create fm-fsel-output-buffer)
        (insert "old content"))
      (fm--run-fsel nil nil "new")
      (with-current-buffer fm-fsel-output-buffer
        (expect (buffer-string) :not :to-match "old content"))))
  
  (it "signals error when fsel executable not found"
    (let ((fm-fsel-executable "nonexistent-command-xyz123"))
      (expect (fm--run-fsel nil nil "-l") :to-throw 'error))))

(describe "fm--run-fsel with real fsel utility"
  
  (before-each
    ;; Clear selection before each test
    (when (locate-file "fsel" exec-path)
      (call-process "fsel" nil nil nil "-c"))
    (when (get-buffer fm-fsel-output-buffer)
      (kill-buffer fm-fsel-output-buffer))
    (when (get-buffer fm-fsel-error-buffer)
      (kill-buffer fm-fsel-error-buffer)))
  
  (after-each
    ;; Clean up selection after tests
    (when (locate-file "fsel" exec-path)
      (call-process "fsel" nil nil nil "-c")))
  
  (it "executes fsel -l command successfully"
    (when (locate-file "fsel" exec-path)
      (expect (fm--run-fsel nil nil "-l") :to-equal 0)))
  
  (it "returns non-zero exit status for invalid arguments"
    (when (locate-file "fsel" exec-path)
      (let ((status (fm--run-fsel nil nil "-invalid-xyz-option")))
        (expect status :not :to-equal 0))))
  
  (it "adds test files to selection"
    (when (locate-file "fsel" exec-path)
      (let ((test-file (car fm-test-files)))
        (expect (fm--run-fsel nil nil test-file) :to-equal 0))))
  
  (it "handles multiple test files"
    (when (locate-file "fsel" exec-path)
      (apply #'fm--run-fsel nil nil fm-test-files)
      (expect (fm--run-fsel nil nil "-l") :to-equal 0)
      (with-current-buffer fm-fsel-output-buffer
        (expect (buffer-string) :to-match "test1")
        (expect (buffer-string) :to-match "test2")
        (expect (buffer-string) :to-match "test3")))))

(describe "fm-append API"
  
  (before-each
    (when (locate-file "fsel" exec-path)
      (call-process "fsel" nil nil nil "-c"))
    (when (get-buffer fm-fsel-output-buffer)
      (kill-buffer fm-fsel-output-buffer)))
  
  (after-each
    (when (locate-file "fsel" exec-path)
      (call-process "fsel" nil nil nil "-c")))
  
  (it "accepts list of paths"
    (let ((fm-fsel-executable "echo"))
      (expect (fm-append '("/path/1" "/path/2")) :to-equal 0)))
  
  (it "passes paths as separate arguments"
    (let ((fm-fsel-executable "echo"))
      (fm-append '("/etc/fstab" "/etc/hosts"))
      (with-current-buffer fm-fsel-output-buffer
        (expect (string-trim (buffer-string)) 
                :to-equal "/etc/fstab /etc/hosts"))))
  
  (it "appends test files to selection"
    (when (locate-file "fsel" exec-path)
      (fm-append (list (car fm-test-files)))
      (expect (fm--run-fsel nil nil "-l") :to-equal 0)
      (with-current-buffer fm-fsel-output-buffer
        (expect (buffer-string) :to-match "test1"))))
  
  (it "appends multiple test files"
    (when (locate-file "fsel" exec-path)
      (fm-append fm-test-files)
      (expect (fm--run-fsel nil nil "-l") :to-equal 0)
      (with-current-buffer fm-fsel-output-buffer
        (let ((content (buffer-string)))
          (expect content :to-match "test1")
          (expect content :to-match "test2")
          (expect content :to-match "test3"))))))

(describe "fm-replace API"
  
  (before-each
    (when (locate-file "fsel" exec-path)
      (call-process "fsel" nil nil nil "-c"))
    (when (get-buffer fm-fsel-output-buffer)
      (kill-buffer fm-fsel-output-buffer)))
  
  (after-each
    (when (locate-file "fsel" exec-path)
      (call-process "fsel" nil nil nil "-c")))
  
  (it "passes -r flag with paths"
    (let ((fm-fsel-executable "echo"))
      (fm-replace '("/path/1" "/path/2"))
      (with-current-buffer fm-fsel-output-buffer
        (expect (buffer-string) :to-match "-r"))))
  
  (it "replaces selection with test files"
    (when (locate-file "fsel" exec-path)
      ;; First add some files
      (fm-append fm-test-files)
      ;; Then replace with folder files
      (fm-replace fm-test-folder-files)
      (expect (fm--run-fsel nil nil "-l") :to-equal 0)
      (with-current-buffer fm-fsel-output-buffer
        (let ((content (buffer-string)))
          (expect content :to-match "folder/test1")
          (expect content :to-match "folder/test2")
          (expect content :to-match "folder/test3")))))
  
  (it "replaces empty selection"
    (when (locate-file "fsel" exec-path)
      (fm-replace (list (car fm-test-files)))
      (expect (fm--run-fsel nil nil "-l") :to-equal 0)
      (with-current-buffer fm-fsel-output-buffer
        (expect (buffer-string) :to-match "test1")))))

(describe "fm-get-list API"
  
  (before-each
    (when (locate-file "fsel" exec-path)
      (call-process "fsel" nil nil nil "-c"))
    (when (get-buffer fm-fsel-output-buffer)
      (kill-buffer fm-fsel-output-buffer)))
  
  (after-each
    (when (locate-file "fsel" exec-path)
      (call-process "fsel" nil nil nil "-c")))
  
  (it "returns current selection as string"
    (when (locate-file "fsel" exec-path)
      (fm-append fm-test-files)
      (let ((list (fm-get-list)))
        (expect list :to-match "test1")
        (expect list :to-match "test2")
        (expect list :to-match "test3"))))
  
  (it "returns empty string for empty selection"
    (when (locate-file "fsel" exec-path)
      (let ((list (fm-get-list)))
        (expect (string-trim list) :to-equal "")))))

(describe "fm-clear API"
  
  (before-each
    (when (locate-file "fsel" exec-path)
      (call-process "fsel" nil nil nil "-c"))
    (when (get-buffer fm-fsel-output-buffer)
      (kill-buffer fm-fsel-output-buffer)))
  
  (after-each
    (when (locate-file "fsel" exec-path)
      (call-process "fsel" nil nil nil "-c")))
  
  (it "executes successfully with mock"
    (let ((fm-fsel-executable "echo"))
      (expect (fm-clear) :to-equal 0)))
  
  (it "clears selection with real fsel"
    (when (locate-file "fsel" exec-path)
      ;; Add some files first
      (fm-append fm-test-files)
      ;; Verify they were added
      (let ((list-before (fm-get-list)))
        (expect list-before :to-match "test1"))
      ;; Clear selection
      (fm-clear)
      ;; Verify selection is empty
      (let ((list-after (fm-get-list)))
        (expect (string-trim list-after) :to-equal ""))))
  
  (it "works on empty selection"
    (when (locate-file "fsel" exec-path)
      (expect (fm-clear) :to-equal 0)
      (let ((list (fm-get-list)))
        (expect (string-trim list) :to-equal "")))))

;;; fm-test.el ends here