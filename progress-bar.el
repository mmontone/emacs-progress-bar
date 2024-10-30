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
(require 'progress-display)

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

(defcustom progress-bar-min-steps 0
  "Minimum number of steps for progress bars to be displayed."
  :type 'integer
  :group 'progress-bar)

(defcustom progress-bar-format-string " [%d of %d](%d%%%%)"
  "String for formatting the progress bar.
Arguments passed are current-step, total-steps and completed percentage.
Consider using field number arguments for more flexibility.
See `format' documentation."
  :type 'string
  :group 'progress-bar)

(defcustom progress-bar-min-time 0.2
  "The minimum time interval between progress bar displays."
  :type 'float
  :group 'progress-bar)

(defcustom progress-bar-min-change 1
  "The minimum percentage change required between progress bar displays."
  :type 'integer
  :group 'progress-bar)

(defcustom progress-bar-message-display-layout
  'concatenate
  "How to display messages when in a progress bar scope.
If `concatenate', the message is concatenated to the right of the progress bar.
If `newline', the message is inserted after a new line.
If `dynamic', the message is either concatenated or inserted after a new line
depending on its length."
  :type '(choice (const concatenate)
                 (const newline)
                 (const dynamic))
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

(defvar progress-bar--message (symbol-function 'message))

(defun progress-bar--display (progress-bar)
  "Display PROGRESS-BAR in echo-area."
  (with-slots (total-steps min-time displayed-time min-change created-time displayed-percentage)
      progress-bar
    (let ((now (float-time))
          (percentage (progress-bar-percentage progress-bar)))
      (when (or (progress-bar-completed-p progress-bar)
                (and (>= total-steps progress-bar-min-steps)
                     (>= now (+ displayed-time min-time))
                     (>= now (+ created-time progress-bar-display-after-seconds))
                     (>= percentage (+ displayed-percentage min-change))))
        ;; Disable logging, and don't allow resize of echo area while displaying the progress bar
        (let ((message-log-max nil)
              (resize-mini-windows nil))
          (funcall progress-bar--message (progress-bar--display-string progress-bar))
          (setf displayed-time now
                displayed-percentage percentage))))))

(defun progress-bar--display-string (progress-bar)
  "String representation of the PROGRESS-BAR."
  (with-slots (current-step total-steps status-message)
      progress-bar
    (let ((msg (progress-bar-formatted-status-message progress-bar)))
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
            (princ (format progress-bar-format-string current-step total-steps (truncate (* completed 100))))))))))

(defun call-with-progress-bar (progress-bar func)
  "Call FUNC using PROGRESS-BAR.
Sets up special treatment for calls to MESSAGE that may occur when
evaluating FUNC, so that messages are displayed together with the progress bar."
  (if (< (progress-bar-total-steps progress-bar) progress-bar-min-steps)
      ;; If total-steps are not enough, then do nothing with the progress-bar
      (funcall func progress-bar)
    ;; Replace the implementation of `message' temporarily, so that
    ;; messages sent by FUNC are shown together with the progress bar.
    (let ((emacs-message (symbol-function 'message)))
      (cl-flet ((pb-message (msg &rest args)
                  ;; This is only for logging. Can we log the message
                  ;; without calling `message' ?
                  ;;(apply emacs-message msg args)
                  (if (< (float-time) (+ (progress-bar-created-time progress-bar) progress-bar-display-after-seconds))
                      (apply emacs-message msg args)
                    ;; else
                    (let ((message-log-max nil))
                      (cl-ecase progress-bar-message-display-layout
                        (concatenate
                         (let ((resize-mini-windows nil))
                           (apply emacs-message
                                  (concat (progress-bar--display-string progress-bar) " | " msg)
                                  args)))
                        (newline
                         (let ((resize-mini-windows t))
                           (apply emacs-message
                                  (concat (progress-bar--display-string progress-bar) "\n" msg)
                                  args)))
                        (dynamic ;; FIXME: improve
                         (let ((resize-mini-windows t))
                           (apply emacs-message
                                  (concat (progress-bar--display-string progress-bar) " | " msg)
                                  args))))))))
        (cl-letf (((symbol-function #'message) #'pb-message))
          (funcall func progress-bar))))))

(defmacro with-progress-bar (spec &rest body)
  "Create a PROGRESS-BAR binding SPEC in BODY scope.
SPEC has either the form (VAR PROGRESS-BAR-INSTANCE) or (VAR &rest INITARGS), with
INITARGS used for creating a `progress-bar'.
This macros sets up special treatment for calls to MESSAGE that may ocurr in BODY,
so that messages are displayed together with the progress bar."
  (declare (indent 2))
  (cl-destructuring-bind (var &rest initargs) spec
    (if (= (length initargs) 1)
        `(let ((,var ,(car initargs)))
           (call-with-progress-bar ,var (lambda (,var) ,@body)))
      `(let ((,var (make-progress-bar ,@initargs)))
         (call-with-progress-bar ,var (lambda (,var) ,@body))))))

(provide 'progress-bar)

;;; progress-bar.el ends here
