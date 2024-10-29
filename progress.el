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
Progress events can be either `started', `updated' or `completed'")

(defvar progress-displayer 'default-progress-displayer
  "Function used to display progress in Emacs.")

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
                 :accessor progress-displayed-time
                 :documentation "Time of last update.")))

(cl-defun make-progress (&key status-message total-steps (current-step 0))
  (make-instance 'progress
                 :status-message status-message
                 :total-steps total-steps
                 :current-step 0))

(defun progress-display (progress)
  (funcall progress-displayer progress))

(defun default-progress-displayer (progress)
  ())

(provide 'progress)

;;; progress.el ends here
