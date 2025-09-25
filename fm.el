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
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex text tools unix vc wp
;; Homepage: https://github.com/grafov/fm
;; Package-Requires: ((emacs "24.3"))
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

(defun fm--run-fsel (buffer &rest args)
  "Execute fsel with ARGS and capture output/errors in buffers.
If BUFFER is provided, use it for stdout instead of *fm-output*.
Stdout is written to *fm-output* (or BUFFER if specified).
Stderr is written to *fm-errors*.
Both buffers are overwritten on each call."
  (unless (locate-file fm-fsel-executable exec-path)
    (error "Utility `fsel' not found at %s" fm-fsel-executable))
  
  (let* ((output-buffer (or buffer "*fm-output*"))
         (error-buffer "*fm-errors*"))
    
    ;; Clear buffers before use
    (with-current-buffer (get-buffer-create output-buffer)
      (erase-buffer))
    (with-current-buffer (get-buffer-create error-buffer)
      (erase-buffer))
    
    ;; Start process with output to buffer
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
                                (with-current-buffer error-buffer
                                  (erase-buffer)))))
      
      ;; Wait for process completion
      (while (eq (process-status process) 'run)
        (sleep-for 0.01))
      
      ;; Return (exit-code . output)
      (cons (process-exit-status process)
            (with-current-buffer (get-buffer output-buffer)
              (buffer-string))))))

(defun fm-get-list ()
  "Get fsel output and display in BUFFER buffer."
  (split-string (cdr (fm--run-fsel "*fm-tmp*"))))

(defun fm-selection-list ()
  "Display current fsel selection in *FM: Selected Files* buffer."
  (interactive)
  (let ((result (fm--run-fsel "*FM: Selected Files*" "-l")))
    (if (zerop (car result))
        (with-current-buffer (get-buffer-create "*FM: Selected Files*")
          (setq buffer-read-only nil)
          (erase-buffer)
          (insert (cdr result))
          (setq buffer-read-only t)
          (goto-char (point-min))
          (display-buffer (current-buffer))
          (switch-to-buffer (current-buffer)))
      (error "Failed to list selection: %s" (cdr result)))))

;; FIXME!
(defun fm-selection-append (paths)
  "Append PATHS to fsel selection.
PATHS should be a list of file paths. Tildes (~) are expanded to user
home directory."
  (let ((expanded-paths (mapcar (lambda (path)
                                  (if (string-prefix-p "~" path)
                                      (expand-file-name path)
                                    path))
                                paths)))
    (let ((result (apply #'fm--run-fsel nil expanded-paths)))
      (if (zerop (car result))
          (message "Paths added to selection")
        (error "Failed to add paths: %s" (cdr result))))))

;; Key bindings
;; (defvar fm-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map (kbd "C-c % l") #'fm-list-selection)
;;     (define-key map (kbd "C-c % c") #'fm-clear-selection)
;;     (define-key map (kbd "C-c % a") #'fm-add-selection)
;;     map)
;;   "Keymap for fm commands.")

(define-minor-mode fm-mode
  "Minor mode for FM commands."
  :require 'fm
  ;;:keymap fm-mode-map
  :global t)

(provide 'fm)
;;; fm.el ends here

(provide 'fm)
;;; fm.el ends here
