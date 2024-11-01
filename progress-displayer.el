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

(defcustom progress-displayer-min-time 0.0
  "Document."
  :type 'float
  :group 'progress)

(defcustom progress-displayer-min-change 1
  "Document."
  :type 'integer
  :group 'progress)

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
  "How to display messages by echo-area-progress-displayers.
If `concatenate', the message is concatenated to the right of the progress bar.
If `newline', the message is inserted after a new line.
If `dynamic', the message is either concatenated or inserted after a new line
depending on its length.
if `none', the message is not displayed."
  :type '(choice (const concatenate)
                 (const newline)
                 (const dynamic)
                 (const none))
  :group 'progress)

(defvar progress-displayer-update-handlers
  (make-hash-table :weakness 'key)
  "A table with progress update handlers for displayers.")

(defvar progress-displayer-class 'minimal-message-progress-displayer
  "The progress-displayer class to instantiate to display progresses.")

(defun progress-displayer-update-function (event progress)
  "Handle progress updates and handle their display."

  (when (and (eql event 'started) progress-displayer-class)
    ;; If starting, create a progress-displayer and register an update handler
    (let ((progress-displayer (make-instance progress-displayer-class
                                             :progress progress)))
      (puthash progress (progress-displayer-update-handler progress-displayer)
               progress-displayer-update-handlers)))

  ;; Send the event
  (let ((update-handler (gethash progress progress-displayer-update-handlers)))
    (when update-handler
      (funcall update-handler event progress))))

;; Register to receive progress updates and handle their display:
(add-hook 'progress-update-functions #'progress-displayer-update-function)

(defvar progress-displayer--message (symbol-function 'message))

(defun progress-displayer--message (message &rest args)
  "Use Emacs original `message' function for displaying MESSAGE using ARGS."
  (apply progress-displayer--message message args))

(defclass progress-displayer ()
  ((progress :type progress
             :initarg :progress
             :accessor progress-displayer-progress)
   (min-time
    :initform (symbol-value 'progress-displayer-min-time)
    :type float
    :initarg :min-time
    :accessor progress-displayer-min-time
    :documentation "The minimum time interval between progress bar displays.")
   (min-change
    :initform (symbol-value 'progress-displayer-min-change)
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
  (with-slots (progress min-time displayed-time min-change displayed-percentage)
      progress-displayer
    (with-slots (total-steps created-time) progress
      (let ((now (float-time))
            (percentage (progress-percentage progress)))
        ;; Progress is not displayed unless the following conditions are met
        (when (or (progress-completed-p progress)
                  (and (>= total-steps progress-displayer-min-steps)
                       (>= now (+ displayed-time min-time))
                       (>= now (+ created-time progress-displayer-after-seconds))
                       (>= percentage (+ displayed-percentage min-change))))
          (cl-call-next-method))))))

(cl-defmethod progress-displayer-update-handler (progress-displayer)
  (lambda (_event _progress)
    (progress-displayer-display-progress progress-displayer)))

(defclass echo-area-progress-displayer (progress-displayer)
  ()
  (:documentation "A `progress-displayer' that uses the echo area for displaying progress."))

(cl-defgeneric progress-displayer-update-handler (progress-displayer)
  "Return a `progress' event handler for PROGRESS-DISPLAYER.")

(cl-defmethod progress-displayer-update-handler ((progress-displayer echo-area-progress-displayer))
  (let ((emacs-message (symbol-function #'message))
        (progress (progress-displayer-progress progress-displayer)))
    (cl-flet ((progress-displayer-message (msg &rest args)
                ;; This is only for logging. Can we log the message
                ;; without calling `message' ?
                ;;(apply emacs-message msg args)
                (if (< (float-time) (+ (progress-created-time progress) progress-displayer-after-seconds))
                    (apply emacs-message msg args)
                  ;; else
                  (let ((message-log-max nil))
                    (cl-ecase progress-displayer-message-layout
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
      (lambda  (event progress)
        (cl-ecase event
          (started
           (fset #'message #'progress-displayer-message)
           (progress-displayer--message (progress-displayer-display-progress progress-displayer)))
          (updated
           (progress-displayer--message (progress-displayer-display-progress progress-displayer)))
          (completed
           ;; Restore the Emacs `message' native function.
           (fset #'message emacs-message)
           (progress-displayer--message (progress-displayer-display-progress progress-displayer)))
          (stopped
           ;; Restore the Emacs `message' native function.
           (fset #'message emacs-message)))))))

(defclass minimal-message-progress-displayer (echo-area-progress-displayer)
  ()
  (:documentation "Very basic progress displayer."))

(cl-defmethod progress-displayer-display-progress ((progress-displayer minimal-message-progress-displayer))
  (let ((progress (progress-displayer-progress progress-displayer)))
    (with-slots (status-message current-step total-steps) progress
      (with-output-to-string
        (when status-message
          (princ (progress-formatted-status-message progress))
          (princ " "))
        (princ (format "[%d/%d]" current-step total-steps))
        (princ " ")
        (princ (progress-percentage progress))
        (princ "%%")))))

(provide 'progress-displayer)

;;; progress-displayer.el ends here
