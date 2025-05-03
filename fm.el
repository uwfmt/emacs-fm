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

(require 'dired)

(defgroup fm nil
  "Interface for fsel utility."
  :group 'files)

(defcustom fm-fsel-executable "fsel"
  "Path to fsel executable."
  :type 'string
  :group 'fm)

(defun fm--run-fsel (command &key buffer &rest args)
  "Run fsel COMMAND with ARGS.
Returns (exit-code . output).
If BUFFER is provided, use it for output; otherwise, default to \"*fsel-buffer*\"."
  (unless (locate-file fm-fsel-executable exec-path)
    (error "Utility `fsel' not found at %s" fm-fsel-executable))
  (let* ((output-buffer (or buffer "*fm-output*"))
         (process (apply #'start-process "fsel-process" nil fm-fsel-executable
                         (append (list command) args)))
         (output (with-current-buffer (get-buffer-create output-buffer)
                   (buffer-string))))
    (while (eq (process-status process) 'run)
      (sleep-for 0.1))
    (cons (process-exit-status process) output)))

(defun fm-list-selection ()
  "Display current fsel selection in *Selected files* buffer and switch to it."
  (interactive)
  (let ((buffer (get-buffer-create "*Selected files*")))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (let ((result (fm--run-fsel "list" :buffer "*Selected files*")))
        (if (zerop (car result))
            (insert (cdr result))
          (insert (format "Error: %s" (cdr result))))
        (setq buffer-read-only t)
        (goto-char (point-min))))
    (display-buffer buffer)
    (switch-to-buffer buffer)))

(defun fm-clear-selection ()
  "Clear current fsel selection."
  (interactive)
  (let ((result (fm--run-fsel "clear" :buffer nil)))
    (if (zerop (car result))
        (message "Selection cleared")
      (error "Failed to clear selection: %s" (cdr result)))))

(defun fm-add-selection (&optional path)
  "Add PATH to fsel selection.
If no PATH provided, tries to get path from:
- active region (if it looks like a path)
- Dired mode (file under cursor)
- minibuffer prompt"
  (interactive)
  (let ((path (or path
                  (and (use-region-p)
                       (buffer-substring (region-beginning) (region-end)))
                  (if (derived-mode-p 'dired-mode)
                      (dired-get-file-for-visit)
                    (read-file-name "Add to selection: ")))))
    (if (file-exists-p path)
        (let ((result (fm--run-fsel "add" path :buffer nil)))
          (if (zerop (car result))
              (message "Added %s to selection" path)
            (error "Failed to add %s: %s" path (cdr result))))
      (error "Invalid path: %s" path))))

;; Key bindings
;; (defvar fm-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map (kbd "C-c % l") #'fm-list-selection)
;;     (define-key map (kbd "C-c % c") #'fm-clear-selection)
;;     (define-key map (kbd "C-c % a") #'fm-add-selection)
;;     map)
;;   "Keymap for fm commands.")

(define-minor-mode fm-mode
  "Minor mode for fm commands."
  :require 'fm
  :keymap fm-mode-map
  :global t)

(provide 'fm)
;;; fm.el ends here

(provide 'fm)
;;; fm.el ends here
