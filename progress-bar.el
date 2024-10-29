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

(defcustom progress-bar-display-after-seconds 0
  "Display progress bars only after this number of seconds have passed."
  :type 'float
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

(defvar progress-bar-update-functions '()
  "An abnormal hook for getting notified of progress bar updates.
Functions get called with a progress bar event, and a progress-bar instance.
Progress bar events can be either `started', `updated' or `completed'")

(defclass progress-bar ()
  ((status-message nil
                   :documentation "The status-message can be either a status-formatter or a list of three status-formatters, the first applied when the progress-bar starts, the second applied for each element processed, the third when the progress-bar completes.
A status-formatter is either a string or a function that takes a progress-bar instance and returns a string.")
   (total-steps nil :type integer)
   (current-step 0 :type integer)
   (min-time progress-bar-min-time
             :type float
             :documentation "The minimum time interval between progress bar displays.")
   (min-change progress-bar-min-change
               :type integer
               :documentation "The minimum percentage change between progress bar displays.")
   (data nil :documentation "Extra data stored in the progress-bar instance for convenience.
Often contains current element being processed.")
   (created-time (float-time))
   (displayed-time 0.0
                   :type float
                   :documentation "Time of last display.")
   (displayed-percentage 0 :type integer
                         :documentation "Last percentage displayed.")))

(defun progress-bar-starting-p (progress-bar)
  "Return T if PROGRESS-BAR is starting and has not yet processed any element."
  (with-slots (current-step data) progress-bar
    (and (zerop current-step) (null data))))

(defun progress-bar-completed-p (progress-bar)
  "Return T if PROGRESS-BAR has completed."
  (with-slots (current-step total-steps) progress-bar
    (= current-step total-steps)))

(defun progress-bar-notify (event progress-bar)
  "Notify EVENT for PROGRESS-BAR.
See `progress-bar-update-functions' hook."
  (dolist (hook progress-bar-update-functions)
    (funcall hook event progress-bar)))

(defun progress-bar-update (progress-bar &rest args)
  "Update PROGRESS-BAR and display it.
ARGS is a property-list of slot-name and value.

Example:
(progress-bar-update pg 'current-step 2 'data 'foo)"
  (cl-loop for (slot value) on args by 'cddr
           do (setf (slot-value progress-bar slot) value))
  (progress-bar--display progress-bar)
  (if (progress-bar-completed-p progress-bar)
      (progress-bar-notify 'completed progress-bar)
    (progress-bar-notify 'updated progress-bar)))

(defun progress-bar-incf (progress-bar &optional increment display)
  "Increment step in PROGRESS-BAR."
  (let ((inc (or increment 1)))
    (with-slots (current-step total-steps) progress-bar
      (when (and total-steps (> (+ current-step inc) total-steps))
        (error "current-step > total-steps"))
      (cl-incf current-step inc)
      (when display
        (progress-bar--display progress-bar))
      (progress-bar-notify 'updated progress-bar))))

(defvar progress-bar--message (symbol-function 'message))

(defun progress-bar-percentage (progress-bar)
  "Current completion percentage of PROGRESS-BAR."
  (if (progress-bar-completed-p progress-bar)
      100
    (with-slots (current-step total-steps) progress-bar
      (truncate (* (/ current-step (float total-steps)) 100)))))

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

(defun progress-bar--format-status-message (progress-bar message)
  (cl-etypecase message
    ((or symbol function)
     (funcall message progress-bar))
    (string message)))

(defun progress-bar-formatted-status-message (progress-bar)
  "Get formatted status message of PROGRESS-BAR."
  (with-slots (status-message) progress-bar
    (cl-etypecase status-message
      (null nil)
      (list (cl-destructuring-bind (starting-message processing-message completed-message) status-message
              (progress-bar--format-status-message
               progress-bar
               (cond
                ((progress-bar-starting-p progress-bar)
                 starting-message)
                ((progress-bar-completed-p progress-bar)
                 completed-message)
                (t processing-message)))))
      (t
       (progress-bar--format-status-message progress-bar status-message)))))

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

;; Utilities

(defun mapc-with-progress-bar (func sequence &rest args)
  "Like `mapc' but using a progress-bar."
  (let ((progress-bar (if (= (length args) 1)
                          (car args)
                        (apply #'make-progress-bar
                               :total-steps (length sequence)
                               :current-step 0
                               args))))
    (with-progress-bar (progress-bar progress-bar)
        (progress-bar-notify 'started progress-bar)
      (dolist (x sequence)
        (setf (progress-bar-data progress-bar) x)
        (funcall func x)
        (progress-bar-incf progress-bar 1 t))
      (setf (progress-bar-data progress-bar) nil)
      (progress-bar--display progress-bar)
      (progress-bar-notify 'completed progress-bar))))

(defmacro dolist-with-progress-bar (spec &rest body)
  "Like DOLIST but displaying a progress-bar as items in the list are processed.
ARGS are arguments for `make-progress-bar'.

\(fn (VAR LIST ARGS...) BODY...)

Example:

\(dolist-with-progress-bar
   (x (cl-loop for i from 1 to 30 collect i)
      :status-message \"Working ...\")
   (sit-for 0.3))"
  (declare (indent 2))
  (cl-destructuring-bind (var list &rest args) spec
    `(mapc-with-progress-bar (lambda (,var) ,@body) ,list ,@args)))

(defmacro dotimes-with-progress-bar (spec &rest body)
  "Like `dotimes' but with a progress bar."
  (declare (indent 2))
  (let ((progress-bar (gensym "progress-bar-")))
    (cl-destructuring-bind (var times &rest args) spec
      `(let ((,progress-bar ,(if (= (length args) 1)
                                 (car args)
                               `(make-progress-bar
                                 :total-steps ,times
                                 :current-step 0
                                 ,@args))))
         (with-progress-bar (,progress-bar ,progress-bar)
             (progress-bar-notify 'started ,progress-bar)
           (dotimes (,var ,times)
             (setf (progress-bar-data ,progress-bar) ,var)
             ,@body
             (progress-bar-incf ,progress-bar 1 t))
           (setf (progress-bar-data ,progress-bar) nil)
           (progress-bar--display ,progress-bar)
           (progress-bar-notify 'completed ,progress-bar))))))

(provide 'progress-bar)

;;; progress-bar.el ends here
