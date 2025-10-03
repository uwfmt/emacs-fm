;;; fm-dired-test.el --- Tests for fm-dired.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Alexander Grafov
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

(require 'buttercup)
(require 'fm)
(require 'fm-dired)
(require 'dired)

(defvar fm-dired-test-temp-dir nil
  "Temporary directory for dired tests.")

(defvar fm-dired-test-source-dir nil
  "Source directory for test files.")

(defvar fm-dired-test-target-dir nil
  "Target directory for copy/move operations.")

(defun fm-dired-test--create-test-file (path content)
  "Create test file at PATH with CONTENT."
  (with-temp-file path
    (insert content)))

(defun fm-dired-test--setup-test-dirs ()
  "Create temporary directory structure for tests."
  (setq fm-dired-test-temp-dir (make-temp-file "fm-dired-test" t))
  (setq fm-dired-test-source-dir (expand-file-name "source" fm-dired-test-temp-dir))
  (setq fm-dired-test-target-dir (expand-file-name "target" fm-dired-test-temp-dir))
  (make-directory fm-dired-test-source-dir t)
  (make-directory fm-dired-test-target-dir t)
  
  ;; Create test files in source directory
  (fm-dired-test--create-test-file 
   (expand-file-name "file1.txt" fm-dired-test-source-dir)
   "content1")
  (fm-dired-test--create-test-file 
   (expand-file-name "file2.txt" fm-dired-test-source-dir)
   "content2")
  (fm-dired-test--create-test-file 
   (expand-file-name "file3.txt" fm-dired-test-source-dir)
   "content3"))

(defun fm-dired-test--cleanup-test-dirs ()
  "Remove temporary test directories."
  (when (and fm-dired-test-temp-dir
             (file-exists-p fm-dired-test-temp-dir))
    (delete-directory fm-dired-test-temp-dir t))
  (setq fm-dired-test-temp-dir nil
        fm-dired-test-source-dir nil
        fm-dired-test-target-dir nil))

(defun fm-dired-test--get-test-files ()
  "Return list of test file paths in source directory."
  (list (expand-file-name "file1.txt" fm-dired-test-source-dir)
        (expand-file-name "file2.txt" fm-dired-test-source-dir)
        (expand-file-name "file3.txt" fm-dired-test-source-dir)))

(describe "fm-dired-select"
  
  (before-each
    (fm-dired-test--setup-test-dirs)
    ;; Clear fsel selection
    (when (locate-file "fsel" exec-path)
      (call-process "fsel" nil nil nil "-c")))
  
  (after-each
    (fm-dired-test--cleanup-test-dirs)
    ;; Clean up fsel selection
    (when (locate-file "fsel" exec-path)
      (call-process "fsel" nil nil nil "-c"))
    ;; Kill any dired buffers
    (when (get-buffer (file-name-as-directory fm-dired-test-source-dir))
      (kill-buffer (get-buffer (file-name-as-directory fm-dired-test-source-dir)))))
  
  (it "adds marked files to fsel selection"
    (when (locate-file "fsel" exec-path)
      (let ((test-files (fm-dired-test--get-test-files)))
        ;; Open dired buffer
        (dired fm-dired-test-source-dir)
        ;; Mark files
        (dired-mark-files-regexp "file[12]\\.txt")
        ;; Call fm-dired-select
        (fm-dired-select)
        ;; Verify files in fsel
        (let ((selection (fm-get-list)))
          (expect selection :to-have-same-items-as 
                  (list (expand-file-name "file1.txt" fm-dired-test-source-dir)
                        (expand-file-name "file2.txt" fm-dired-test-source-dir)))))))
  
  (it "replaces existing fsel selection"
    (when (locate-file "fsel" exec-path)
      ;; Add initial files to fsel
      (fm-append (list (expand-file-name "file3.txt" fm-dired-test-source-dir)))
      ;; Open dired and mark different files
      (dired fm-dired-test-source-dir)
      (dired-mark-files-regexp "file1\\.txt")
      ;; Call fm-dired-select
      (fm-dired-select)
      ;; Verify only newly selected file is in fsel
      (let ((selection (fm-get-list)))
        (expect selection :to-contain (expand-file-name "file1.txt" fm-dired-test-source-dir))
        (expect (length selection) :to-equal 1)))))

(describe "fm-dired-clear"
  
  (before-each
    (fm-dired-test--setup-test-dirs)
    (when (locate-file "fsel" exec-path)
      (call-process "fsel" nil nil nil "-c")))
  
  (after-each
    (fm-dired-test--cleanup-test-dirs)
    (when (locate-file "fsel" exec-path)
      (call-process "fsel" nil nil nil "-c"))
    (when (get-buffer (file-name-as-directory fm-dired-test-source-dir))
      (kill-buffer (get-buffer (file-name-as-directory fm-dired-test-source-dir)))))
  
  (it "clears fsel selection"
    (when (locate-file "fsel" exec-path)
      ;; Add files to fsel
      (fm-append (fm-dired-test--get-test-files))
      ;; Verify files are in selection
      (expect (fm-get-list) :not :to-equal nil)
      ;; Call fm-dired-clear
      (dired fm-dired-test-source-dir)
      (fm-dired-clear)
      ;; Verify selection is empty
      (let ((selection (fm-get-list)))
        (expect selection :to-equal nil))))
  
  (it "unmarks all files in dired"
    (when (locate-file "fsel" exec-path)
      ;; Open dired and mark files
      (dired fm-dired-test-source-dir)
      (dired-mark-files-regexp ".*\\.txt")
      ;; Verify files are marked
      (expect (dired-get-marked-files) :not :to-equal nil)
      ;; Call fm-dired-clear
      (fm-dired-clear)
      ;; Verify no files are marked (only current directory)
      (let ((marked (dired-get-marked-files)))
        (expect (length marked) :to-equal 1)
        (expect (car marked) :to-equal (file-name-as-directory fm-dired-test-source-dir))))))

(describe "fm-dired-copy-here"
  
  (before-each
    (fm-dired-test--setup-test-dirs)
    (when (locate-file "fsel" exec-path)
      (call-process "fsel" nil nil nil "-c")))
  
  (after-each
    (fm-dired-test--cleanup-test-dirs)
    (when (locate-file "fsel" exec-path)
      (call-process "fsel" nil nil nil "-c"))
    (when (get-buffer (file-name-as-directory fm-dired-test-target-dir))
      (kill-buffer (get-buffer (file-name-as-directory fm-dired-test-target-dir)))))
  
  (it "copies files from fsel to current directory"
    (when (locate-file "fsel" exec-path)
      ;; Add source files to fsel
      (fm-append (list (expand-file-name "file1.txt" fm-dired-test-source-dir)
                       (expand-file-name "file2.txt" fm-dired-test-source-dir)))
      ;; Open target directory in dired
      (dired fm-dired-test-target-dir)
      ;; Copy files
      (cl-letf (((symbol-function 'dired-query) (lambda (&rest _) t)))
        (fm-dired-copy-here))
      ;; Verify files exist in target
      (expect (file-exists-p (expand-file-name "file1.txt" fm-dired-test-target-dir)) 
              :to-be-truthy)
      (expect (file-exists-p (expand-file-name "file2.txt" fm-dired-test-target-dir)) 
              :to-be-truthy)
      ;; Verify source files still exist
      (expect (file-exists-p (expand-file-name "file1.txt" fm-dired-test-source-dir)) 
              :to-be-truthy)))
  
  (it "preserves file content when copying"
    (when (locate-file "fsel" exec-path)
      ;; Add source file to fsel
      (fm-append (list (expand-file-name "file1.txt" fm-dired-test-source-dir)))
      ;; Open target directory in dired
      (dired fm-dired-test-target-dir)
      ;; Copy file
      (cl-letf (((symbol-function 'dired-query) (lambda (&rest _) t)))
        (fm-dired-copy-here))
      ;; Verify content matches
      (let ((source-content (with-temp-buffer
                             (insert-file-contents 
                              (expand-file-name "file1.txt" fm-dired-test-source-dir))
                             (buffer-string)))
            (target-content (with-temp-buffer
                             (insert-file-contents 
                              (expand-file-name "file1.txt" fm-dired-test-target-dir))
                             (buffer-string))))
        (expect target-content :to-equal source-content)))))

(describe "fm-dired-move-here"
  
  (before-each
    (fm-dired-test--setup-test-dirs)
    (when (locate-file "fsel" exec-path)
      (call-process "fsel" nil nil nil "-c")))
  
  (after-each
    (fm-dired-test--cleanup-test-dirs)
    (when (locate-file "fsel" exec-path)
      (call-process "fsel" nil nil nil "-c"))
    (when (get-buffer (file-name-as-directory fm-dired-test-target-dir))
      (kill-buffer (get-buffer (file-name-as-directory fm-dired-test-target-dir)))))
  
  (it "moves files from fsel to current directory"
    (when (locate-file "fsel" exec-path)
      ;; Add source files to fsel
      (fm-append (list (expand-file-name "file1.txt" fm-dired-test-source-dir)))
      ;; Open target directory in dired
      (dired fm-dired-test-target-dir)
      ;; Move file
      (cl-letf (((symbol-function 'dired-query) (lambda (&rest _) t)))
        (fm-dired-move-here))
      ;; Verify file exists in target
      (expect (file-exists-p (expand-file-name "file1.txt" fm-dired-test-target-dir)) 
              :to-be-truthy)
      ;; Verify source file no longer exists
      (expect (file-exists-p (expand-file-name "file1.txt" fm-dired-test-source-dir)) 
              :not :to-be-truthy)))
  
  (it "clears fsel selection after move"
    (when (locate-file "fsel" exec-path)
      ;; Add source file to fsel
      (fm-append (list (expand-file-name "file2.txt" fm-dired-test-source-dir)))
      ;; Verify file is in selection
      (expect (fm-get-list) :not :to-equal nil)
      ;; Open target directory and move
      (dired fm-dired-test-target-dir)
      (cl-letf (((symbol-function 'dired-query) (lambda (&rest _) t)))
        (fm-dired-move-here))
      ;; Verify selection is cleared
      (expect (fm-get-list) :to-equal nil))))

(describe "fm-dired-remove"
  
  (before-each
    (fm-dired-test--setup-test-dirs)
    (when (locate-file "fsel" exec-path)
      (call-process "fsel" nil nil nil "-c")))
  
  (after-each
    (fm-dired-test--cleanup-test-dirs)
    (when (locate-file "fsel" exec-path)
      (call-process "fsel" nil nil nil "-c")))
  
  (it "deletes files from fsel selection"
    (when (locate-file "fsel" exec-path)
      ;; Add files to fsel
      (let ((test-file (expand-file-name "file1.txt" fm-dired-test-source-dir)))
        (fm-append (list test-file))
        ;; Verify file exists
        (expect (file-exists-p test-file) :to-be-truthy)
        ;; Delete files (mock confirmation)
        (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                  ((symbol-function 'dired-query) (lambda (&rest _) t)))
          (fm-dired-remove))
        ;; Verify file is deleted
        (expect (file-exists-p test-file) :not :to-be-truthy))))
  
  (it "clears fsel selection after deletion"
    (when (locate-file "fsel" exec-path)
      ;; Add files to fsel
      (fm-append (list (expand-file-name "file2.txt" fm-dired-test-source-dir)))
      ;; Verify selection not empty
      (expect (fm-get-list) :not :to-equal nil)
      ;; Delete files (mock confirmation)
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                ((symbol-function 'dired-query) (lambda (&rest _) t)))
        (fm-dired-remove))
      ;; Verify selection is cleared
      (expect (fm-get-list) :to-equal nil)))
  
  (it "does not delete if user cancels confirmation"
    (when (locate-file "fsel" exec-path)
      ;; Add file to fsel
      (let ((test-file (expand-file-name "file3.txt" fm-dired-test-source-dir)))
        (fm-append (list test-file))
        ;; Delete with canceled confirmation
        (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) nil)))
          (fm-dired-remove))
        ;; Verify file still exists
        (expect (file-exists-p test-file) :to-be-truthy)
        ;; Verify selection still has file
        (expect (fm-get-list) :not :to-equal nil)))))

(describe "fm-dired integration"
  
  (it "has fm-dired-mode defined"
    (expect (fboundp 'fm-dired-mode) :to-be-truthy))
  
  (it "has keymap with all bindings"
    (expect (keymapp fm-dired-mode-map) :to-be-truthy)
    (expect (lookup-key fm-dired-mode-map (kbd "* S")) :to-equal 'fm-dired-select)
    (expect (lookup-key fm-dired-mode-map (kbd "* C")) :to-equal 'fm-dired-copy-here)
    (expect (lookup-key fm-dired-mode-map (kbd "* M")) :to-equal 'fm-dired-move-here)
    (expect (lookup-key fm-dired-mode-map (kbd "* D")) :to-equal 'fm-dired-remove)
    (expect (lookup-key fm-dired-mode-map (kbd "* U")) :to-equal 'fm-dired-clear)))

;;; fm-dired-test.el ends here

