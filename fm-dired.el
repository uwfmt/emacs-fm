;;; fm-dired.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Alexander Grafov
;;
;; Author: Alexander Grafov <ag@inet.name>
;; Maintainer: Alexander Grafov <ag@inet.name>
;; Created: Septembro 25, 2025
;; Modified: Septembro 25, 2025
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex text tools unix vc wp
;; Homepage: https://github.com/grafov/fm-dired
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
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
(defun fm-dired-paste ()
  "Copy the items from copy ring to current directory.

With raw prefix argument \\[universal-argument], do not remove
the selection from the stack so it can be copied again.

With numeric prefix argument, copy the n-th selection from the
copy ring."
  (interactive)
  (dired-create-files #'dired-copy-file "Copy" (fm-get-list)
                      #'fm-dired--name-constructor ?C))

;;;###autoload
(defun fm-dired-append ()
  "Place the marked items into the selection."
  (interactive)
  (let ((marked (dired-get-marked-files)))
    (fm-selection-append marked)))

(provide 'fm-dired)
;;; fm-dired.el ends here
