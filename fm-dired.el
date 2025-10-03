;;; fm-dired.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Alexander Grafov
;;
;; Author: Alexander Grafov <ag@inet.name>
;; Maintainer: Alexander Grafov <ag@inet.name>
;; Created: Septembro 25, 2025
;; Modified: Septembro 25, 2025
;; Version: 0.0.1
;; Keywords: files tools unix
;; Homepage: https://github.com/uwfmt/emacs-fm
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Integration with Dired.
;;
;;  Description
;;
;;; Code:

(require 'dired)
(require 'fm)

(defun fm-dired--name-constructor (oldname)
  "Return the new file name corresponding to OLDNAME."
  (concat (dired-current-directory) (file-name-nondirectory oldname)))

;;;###autoload
(defun fm-dired-copy-here ()
  "Copy the items from copy ring to current directory.

With raw prefix argument \\[universal-argument], do not remove
the selection from the stack so it can be copied again.

With numeric prefix argument, copy the n-th selection from the
copy ring."
  (interactive)
  (dired-create-files #'dired-copy-file "Copy" (fm-get-list)
                      #'fm-dired--name-constructor ?C))

;;;###autoload
(defun fm-dired-move-here ()
  "Move the items from selection to current directory.

The files from the selection are moved to the current directory
and the selection is cleared after the operation."
  (interactive)
  (dired-create-files #'dired-rename-file "Move" (fm-get-list)
                      #'fm-dired--name-constructor ?R)
  (fm-clear))

;;;###autoload
(defun fm-dired-select ()
  "Place the marked items into the selection."
  (interactive)
  (let ((marked (dired-get-marked-files)))
    (fm-replace marked)))

;;;###autoload
(defun fm-dired-remove ()
  "Delete files from fsel selection.

Ask for confirmation before deleting. The selection is cleared
after the operation."
  (interactive)
  (let ((files (fm-get-list)))
    (when files
      (when (yes-or-no-p (format "Delete %d file(s) from selection? " (length files)))
        (let ((deletion-list (mapcar (lambda (file) (cons file nil)) files)))
          (dired-internal-do-deletions deletion-list 'always t))
        (fm-clear)
        (message "Deleted %d file(s)" (length files))))))

;;;###autoload
(defun fm-dired-clear ()
  "Clear fsel selection and unmark all files in Dired."
  (interactive)
  (fm-clear)
  (dired-unmark-all-marks)
  (message "Selection and marks cleared"))

;; Key bindings
(defvar fm-dired-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "* S") #'fm-dired-select)
    (define-key map (kbd "* C") #'fm-dired-copy-here)
    (define-key map (kbd "* M") #'fm-dired-move-here)
    (define-key map (kbd "* D") #'fm-dired-remove)
    (define-key map (kbd "* U") #'fm-dired-clear)
    map)
  "Keymap for fm commands.")

;;;###autoload
(define-minor-mode fm-dired-mode
  "Minor mode for FM + Dired commands."
  :require 'fm
  :keymap fm-dired-mode-map
  :lighter " FM")

(provide 'fm-dired)
;;; fm-dired.el ends here
