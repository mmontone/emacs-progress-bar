;;; progress.el --- Model for progress               -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Mariano Montone

;; Author: Mariano Montone <marianomontone@gmail.com>
;; Keywords: tools, convenience, extensions

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

;; Model for progress.

;;; Code:

(require 'cl-lib)
(require 'eieio)

(defvar progress-update-functions '()
  "An abnormal hook for getting notified of progress updates.
Functions get called with a progress event, and a progress instance.
Progress events can be either `started', `updated', `completed' or `stopped'.")

(defclass progress ()
  ((status-message :initform nil
                   :initarg :status-message
                   :accessor progress-status-message
                   :documentation "The status-message can be either a status-formatter or a list of three status-formatters, the first applied when the progress starts, the second applied for each element processed, the third when the progress completes.
A status-formatter is either a string or a function that takes a progress instance and returns a string.")
   (total-steps :type integer
                :initarg :total-steps
                :accessor progress-total-steps)
   (current-step :initform 0
                 :type integer
                 :initarg :current-step
                 :accessor progress-current-step)
   (data :initform nil
         :accessor progress-data
         :documentation "Extra data stored in the progress instance for convenience.
Often contains current element being processed.")
   (created-time :initform (float-time)
                 :accessor progress-created-time)
   (updated-time :initform 0.0
                 :type float
                 :accessor progress-update-time
                 :documentation "Time of last update.")))

(cl-defun make-progress (&key status-message total-steps (current-step 0))
  "Create a `progress' instance."
  (make-instance 'progress
                 :status-message status-message
                 :total-steps total-steps
                 :current-step current-step))

(defun progress-starting-p (progress)
  "Return T if PROGRESS is starting and has not yet processed any element."
  (with-slots (current-step data) progress
    (and (zerop current-step) (null data))))

(defun progress-completed-p (progress)
  "Return T if PROGRESS has completed."
  (with-slots (current-step total-steps) progress
    (= current-step total-steps)))

(defun progress-notify (event progress)
  "Notify EVENT for PROGRESS.
See `progress-update-functions' hook."
  (dolist (hook progress-update-functions)
    (funcall hook event progress)))

(defun progress-percentage (progress)
  "Current completion percentage of PROGRESS."
  (if (progress-completed-p progress)
      100
    (with-slots (current-step total-steps) progress
      (truncate (* current-step 100.0) total-steps))))

(defun progress-update (progress &rest args)
  "Update PROGRESS using ARGS.
ARGS is a property-list of slot-name and value.

Example:
(progress-update pg 'current-step 2 'data 'foo)"
  (cl-loop for (slot value) on args by 'cddr
           do (setf (slot-value progress slot) value))
  (if (progress-completed-p progress)
      (progress-notify 'completed progress)
    (progress-notify 'updated progress)))

(defun progress-incf (progress &optional increment)
  "Increment step in PROGRESS."
  (let ((inc (or increment 1)))
    (with-slots (current-step total-steps) progress
      (when (and total-steps (> (+ current-step inc) total-steps))
        (error "current-step > total-steps"))
      (progress-update progress 'current-step (+ current-step inc)))))

(defun progress--format-status-message (progress message)
  (cl-etypecase message
    (function (funcall message progress))
    (string message)))

(defun progress-formatted-status-message (progress)
  "Get formatted status message of PROGRESS."
  (with-slots (status-message) progress
    (cl-etypecase status-message
      (null nil)
      (list (cl-destructuring-bind (starting-message processing-message completed-message) status-message
              (progress--format-status-message
               progress
               (cond
                ((progress-starting-p progress)
                 starting-message)
                ((progress-completed-p progress)
                 completed-message)
                (t processing-message)))))
      (t
       (progress--format-status-message progress status-message)))))

(defun call-with-progress (progress func)
  "Call FUNC using PROGRESS.
Sets up a context PROGRESS for evaluating FUNC."
  (progress-notify 'started progress)
  (setf (progress-data progress) nil)
  (unwind-protect
      (funcall func)
    (if (progress-completed-p progress)
        (progress-notify 'completed progress)
      (progress-notify 'stopped progress))))

(defmacro with-progress (spec &rest body)
  "Create a `progress' instance binding SPEC in BODY scope.
SPEC has either the form (VAR PROGRESS-INSTANCE) or (VAR &rest INITARGS), with
INITARGS used for creating a `progress' instance."
  (declare (indent 2))
  (cl-destructuring-bind (var &rest initargs) spec
    (if (= (length initargs) 1)
        `(let ((,var ,(car initargs)))
           (call-with-progress ,var (lambda (,var) ,@body)))
      `(let ((,var (make-progress ,@initargs)))
         (call-with-progress ,var (lambda (,var) ,@body))))))

;; Utilities

(defun progress-mapc (func sequence &rest args)
  "Like `mapc' but using a progress-bar."
  (let ((progress (if (= (length args) 1)
                      (car args)
                    (apply #'make-progress
                           :total-steps (length sequence)
                           :current-step 0
                           args))))
    (with-progress (progress)
                   (dolist (x sequence)
                     (setf (progress-data progress) x)
                     (funcall func x)
                     (progress-incf progress)))))

(defmacro progress-dolist (spec &rest body)
  "Like DOLIST but displaying a progress-bar as items in the list are processed.
ARGS are arguments for `make-progress'.

\(fn (VAR LIST ARGS...) BODY...)

Example:

\(progress-dolist
   (x (cl-loop for i from 1 to 30 collect i)
      :status-message \"Working ...\")
   (sit-for 0.3))"
  (declare (indent 2))
  (cl-destructuring-bind (var list &rest args) spec
    `(progress-dolist (lambda (,var) ,@body) ,list ,@args)))

(defmacro progress-dotimes (spec &rest body)
  "Like `dotimes' but with progress."
  (declare (indent 2))
  (let ((progress (gensym "progress-")))
    (cl-destructuring-bind (var times &rest args) spec
      `(let ((,progress ,(if (= (length args) 1)
                             (car args)
                           `(make-progress
                             :total-steps ,times
                             :current-step 0
                             ,@args))))
         (with-progress (,progress)
                        (dotimes (,var ,times)
                          (setf (progress-bar-data ,progress) ,var)
                          ,@body
                          (progress-bar-incf ,progress)))))))

(provide 'progress)

;;; progress.el ends here
