;;; progress-bar.el --- A progress bar in the echo area              -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: Mariano Montone <marianomontone@gmail.com>
;; Keywords: tools, convenience, extensions
;; Version: 0.5
;; Package-Requires: ((emacs "27.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A progress bar in the echo area.
;;
;; This package contains the basic implementation.  For integration of progress-bar
;; into common Emacs commands and behaviors, install progress-bar-integrations package.
;;
;; Usage:
;;
;; The preferred method for using a progress-bar is via the utility functions:
;; `dolist-with-progress-bar', `dotimes-with-progress-bar' and `mapc-with-progress-bar'.
;;
;; Example:
;;
;; (dolist-with-progress-bar (x (cl-loop for i from 1 to 10 collect i)
;;                              :status-message (list "Started ..."
;;                                                    (lambda (pb)
;;                                                      (format "Processing %s..." (progress-bar-data pb)))
;;                                                    "Completed!"))
;;     (sit-for (seq-random-elt '(0.3 0.4 0.5))))
;;
;; TODO:
;; - Consider putting event notification in call-with-progress-bar instead of in the utilities.
;; - Consider implementing progress-bars with no total-steps specified.
;; - Consider an option for hiding the progress-bar display after N seconds after completion.

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'progress)
(require 'progress-displayer)

(defgroup progress-bar nil
  "Progress bar settings."
  :group 'convenience)

;; Chosen from https://en.wikipedia.org/wiki/Block_Elements and inserted using `insert-char' command:

(defcustom progress-bar-char ?▓
  "Character for drawing progress bars."
  :type 'character
  :group 'progress-bar)

(defcustom progress-bar-background-char ?░
  "Character for drawing progress bars background."
  :type 'character
  :group 'progress-bar)

(defcustom progress-bar-width 35
  "Standard width for progress bars."
  :type 'integer
  :group 'progress-bar)

(defcustom progress-bar-format-string " [%d of %d](%d%%%%)"
  "String for formatting the progress bar.
Arguments passed are current-step, total-steps and completed percentage.
Consider using field number arguments for more flexibility.
See `format' documentation."
  :type 'string
  :group 'progress-bar)

(defclass progress-bar (echo-area-progress-displayer)
  ())

(cl-defun make-progress-bar (progress &key (min-time progress-bar-min-time)
                                      (min-change progress-bar-min-change))
  "Create a PROGRESS-BAR instance."
  (make-instance 'progress-bar
                 :progress progress
                 :status-message status-message
                 :total-steps total-steps
                 :current-step current-step
                 :min-time min-time
                 :min-change min-change))

(cl-defmethod progress-displayer-display-progress ((progress-bar progress-bar))
  (with-slots (progress) progress-bar
    (with-slots (current-step total-steps status-message)
        progress
      (let ((msg (progress-formatted-status-message progress)))
        (if (zerop total-steps)
            (or msg "")
          (let* ((completed (/ current-step (float total-steps)))
                 (chars (truncate (* completed progress-bar-width))))
            (with-output-to-string
              (dotimes (_c chars)
                (princ (string progress-bar-char)))
              (dotimes (_c (- progress-bar-width chars))
                (princ (string progress-bar-background-char)))
              (when msg
                (princ " ")
                (princ msg))
              (princ (format progress-bar-format-string current-step total-steps (truncate (* completed 100)))))))))))

(provide 'progress-bar)

;;; progress-bar.el ends here
