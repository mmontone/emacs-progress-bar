;;; progress-displayer.el --- Top-level abstract class for displaying progress  -*- lexical-binding: t; -*-

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

;; Top-level abstract class for progress display

;;; Code:

(require 'progress)

(defcustom progress-displayer-after-seconds 0
  "Display progress bars only after this number of seconds have passed."
  :type 'float
  :group 'progress)

(defcustom progress-displayer-min-steps 0
  "Minimum number of steps for progress to be displayed."
  :type 'integer
  :group 'progress)

(defcustom progress-displayer-message-layout
  'concatenate
  "How to display messages when in a progress display scope.
If `concatenate', the message is concatenated to the right of the progress bar.
If `newline', the message is inserted after a new line.
If `dynamic', the message is either concatenated or inserted after a new line
depending on its length."
  :type '(choice (const concatenate)
                 (const newline)
                 (const dynamic))
  :group 'progress)

(defvar progress-displayer--message (symbol-function 'message))

(defun progress-displayer-message (message &rest args)
  "Use Emacs original `message' function for displaying MESSAGE using ARGS."
  (apply progress-displayer-message message args))

(defclass progress-displayer ()
  ((progress :type progress
             :initarg :progress
             :accessor progress-displayer-progress)
   (min-time ;;:initform progress-bar-min-time
    :type float
    :initarg :min-time
    :accessor progress-displayer-min-time
    :documentation "The minimum time interval between progress bar displays.")
   (min-change ;;:initform progress-bar-min-change
    :initarg :min-change
    :type integer
    :accessor progress-displayer-min-change
    :documentation "The minimum percentage change between progress bar displays.")
   (displayed-time :initform 0.0
                   :type float
                   :accessor progress-displayer-displayed-time
                   :documentation "Time of last display.")
   (displayed-percentage :initform 0 :type integer
                         :accessor progress-displayer-displayed-percentage
                         :documentation "Last percentage displayed.")))

(cl-defgeneric progress-displayer-display-progress (progress-displayer)
  "Specializable generic function for displaying PROGRESS-DISPLAYER.")

(cl-defmethod progress-displayer-display-progress :around (progress-displayer)
  (with-slots (progress total-steps created-time)
      (progress-displayer-progress progress-bar)
    (with-slots (min-time displayed-time min-change displayed-percentage)
        progress-bar
      (let ((now (float-time))
            (percentage (progress-percentage progress-bar)))
        (when (or (progress-completed-p progress-bar)
                  (and (>= total-steps progress-displayer-min-steps)
                       (>= now (+ displayed-time min-time))
                       (>= now (+ created-time progress-displayer-after-seconds))
                       (>= percentage (+ displayed-percentage min-change))))
          (call-next-method))))))

(defclass echo-area-progress-displayer (progress-displayer)
  ())

(cl-defgeneric progress-displayer-call-with-displayer (progress-displayer func)
  "Evaluate FUNC in the scope of PROGRESS-DISPLAYER.")

(cl-defmethod progress-displayer-call-with-displayer (progress-displayer func)
  (progress-displayer-display-progress progress-displayer))

(cl-defmethod progress-displayer-call-with-displayer ((progress-displayer echo-area-progress-displayer) func)
  (let ((emacs-message (symbol-function #'message)))
    (cl-flet ((progress-displayer-message (msg &rest args)
                ;; This is only for logging. Can we log the message
                ;; without calling `message' ?
                ;;(apply emacs-message msg args)
                (if (< (float-time) (+ (progress-created-time progress) progress-display-after-seconds))
                    (apply emacs-message msg args)
                  ;; else
                  (let ((message-log-max nil))
                    (cl-ecase progress-display-message-layout
                      (concatenate
                       (let ((resize-mini-windows nil))
                         (apply emacs-message
                                (concat (progress-displayer-display-progress progress-displayer) " | " msg)
                                args)))
                      (newline
                       (let ((resize-mini-windows t))
                         (apply emacs-message
                                (concat (progress-displayer-display-progress progress-displayer) "\n" msg)
                                args)))
                      (dynamic ;; FIXME: improve
                       (let ((resize-mini-windows t))
                         (apply emacs-message
                                (concat (progress-displayer-display-progress progress-displayer) " | " msg)
                                args)))
                      (none
                       (apply emacs-message (progress-displayer-display-progress progress-displayer) args))
                      )))))
      (cl-letf (((symbol-function #'message) #'progress-displayer-message))
        (funcall func progress)))))

(defun call-with-progress-displayer (progress-displayer func)
  "Call FUNC using PROGRESS-DISPLAYER.
Sets up a context PROGRESS-DISPLAYER for evaluating FUNC."
  (with-slots (progress) progress-displayer
    (if (< (progress-total-steps progress) progress-display-min-steps)
        ;; If total-steps are not enough, then do nothing with the progress-bar
        (funcall func progress)
      ;; Replace the implementation of `message' temporarily, so that
      ;; messages sent by FUNC are shown together with the progress bar.
      (progn
        (progress-notify 'started progress)
        (progress-displayer-call-with-displayer progress-displayer func)
        (setf (progress-data progress) nil)
        (progress-displayer-display-progress progress-displayer)
        (progress-notify 'completed progress)))))

(defmacro with-progress-displayer (spec &rest body)
  "Create a PROGRESS-DISPLAYER binding SPEC in BODY scope.
SPEC has either the form (VAR PROGRESS-BAR-INSTANCE) or (VAR &rest INITARGS), with
INITARGS used for creating a `progress-displayer'."
  (declare (indent 2))
  (cl-destructuring-bind (var &rest args) spec
    (if (= (length initargs) 1)
        `(let ((,var ,(car initargs)))
           (call-with-progress-displayer ,var (lambda (,var) ,@body)))
      `(let ((,var (make-instance ,@args)))
         (call-with-progress-displayer ,var (lambda (,var) ,@body))))))

(provide 'progress-displayer)

;;; progress-displayer.el ends here
