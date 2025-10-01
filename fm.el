;;; fm.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Alexander Grafov
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;; Author: Alexander Grafov <ag@inet.name>
;; Maintainer: Alexander Grafov <ag@inet.name>
;; Created: Majo 04, 2025
;; Modified: Majo 04, 2025
;; Version: 0.0.1
;; Keywords: files tools unix
;; Homepage: https://github.com/uwfmt/emacs-fm
;; Package-Requires: ((emacs "24.4"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Emacs interface for fsel utility. Provides commands to manage file selections:
;; - List current selection in a buffer
;; - Add files to selection from various sources
;; - Clear selection
;;
;;; Code:

(require 'cl-lib)

(defgroup fm nil
  "Interface for fsel utility."
  :group 'files)

(defcustom fm-fsel-executable "fsel"
  "Path to fsel executable."
  :type 'string
  :group 'fm)

(defconst fm-fsel-output-buffer "*fm-output*"
  "Buffer name for raw output (stdout) of fsel utility.")
(defconst fm-fsel-error-buffer "*fm-errors*"
  "Errors buffer for output of errors (stderr) of fsel utility.")

(defun fm--run-fsel (buffer tmpbuf &rest args)
  "Execute fsel with ARGS and capture output/errors in buffers.
If BUFFER is provided, use it for stdout instead of *fm-output*.
Set TMPBUF to non nil for using temporary buffer (with popup).
Stdout is written to *fm-output* (or BUFFER if specified).
Stderr is written to *fm-errors*.
Both buffers are overwritten on each call."
  (unless (locate-file fm-fsel-executable exec-path)
    (error "Utility `fsel' not found at %s" fm-fsel-executable))

  (let* ((output-buffer (or buffer fm-fsel-output-buffer))
         (exit-status 0))

    ;; Clear buffers before use
    (unless tmpbuf
      (with-current-buffer (get-buffer-create output-buffer)
        (setq-local buffer-read-only nil)
        (erase-buffer)))
    (with-current-buffer (get-buffer-create fm-fsel-error-buffer)
      (setq-local buffer-read-only nil)
      (erase-buffer))

    ;; Start process with output to buffer
    (if tmpbuf
        (with-output-to-temp-buffer output-buffer
          (let ((process (apply #'start-process
                                "fsel-process"
                                (current-buffer)
                                fm-fsel-executable
                                args)))
            (set-process-filter process (lambda (p output) (princ output)))
            ;; Redirect stderr to error buffer
            (set-process-sentinel process
                                  (lambda (proc event)
                                    (when (and (eq (process-status proc) 'exit)
                                               (= (process-exit-status proc) 0))
                                      (with-current-buffer fm-fsel-error-buffer
                                        (erase-buffer)))))

            ;; Wait for process completion
            (while (eq (process-status process) 'run)
              (sleep-for 0.01))
            (setq exit-status (process-exit-status process))))

      ;; Else do for regular buffer
      (let ((process (apply #'start-process
                            "fsel-process"
                            output-buffer
                            fm-fsel-executable
                            args)))

        ;; Redirect stderr to error buffer
        (set-process-sentinel process
                              (lambda (proc event)
                                (when (and (eq (process-status proc) 'exit)
                                           (= (process-exit-status proc) 0))
                                  (with-current-buffer fm-fsel-error-buffer
                                    (erase-buffer)))))

        ;; Wait for process completion
        (while (eq (process-status process) 'run)
          (sleep-for 0.01))
        (setq exit-status (process-exit-status process))
        (with-current-buffer (get-buffer-create output-buffer) (setq-local buffer-read-only t))
        (with-current-buffer (get-buffer-create fm-fsel-error-buffer) (setq-local buffer-read-only t))))

    ;; Return exit-code
    exit-status))

;;
;;;
;;;; API
;;;
;;

(defun fm-get-list ()
  "Get current files selection as a list of paths."
  (fm--run-fsel nil nil)
  (with-current-buffer fm-fsel-output-buffer
    (let ((content (string-trim (buffer-string))))
      (if (string-empty-p content)
          nil
        (split-string content "\n" t)))))

(defun fm-append (paths)
  "Append PATHS to fsel selection.
PATHS should be a list of file paths."
  (apply #'fm--run-fsel nil nil paths))

(defun fm-replace (paths)
  "Replace current fsel selection with PATHS.
PATHS should be a list of file paths."
  (apply #'fm--run-fsel nil nil "-r" paths))

(defun fm-clear ()
  "Clear current fsel selection."
  (fm--run-fsel nil nil "-c"))


;;
;;;
;;;; Interactive functitons for users
;;;
;;

;;;###autoload
(defun fm-selection-list ()
  "Display current fsel selection in a buffer."
  (interactive)
  (fm--run-fsel "*FM: Selected Files*" t "-l"))

;;;###autoload
(defun fm-selection-clear ()
  "Clear current fsel selection."
  (interactive)
  (let ((result (fm-clear)))
    (if (zerop result)
        (message "Selection cleared"))))

;;;###autoload
(defun fm-selection-append (paths)
  "Append PATHS to fsel selection.
PATHS should be a list of file paths."
  (interactive)
  (let ((expanded-paths (mapcar (lambda (path)
                                  (if (string-prefix-p "~" path)
                                      (expand-file-name path)
                                    path))
                                paths)))
    (let ((result (apply #'fm--run-fsel nil nil expanded-paths)))
      (if (zerop result)
          (message "Paths added to selection")
        (error "Failed to add paths")))))

(provide 'fm)
;;; fm.el ends here
