;;; progress-reporter.el --- Emacs default progress reporter  -*- lexical-binding: t; -*-

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

;; Emacs default progress reporter

;;; Code:

(require 'eieio)
(require 'cl-lib)
(require 'progress)
(require 'progress-displayer)

(defvar progress-reporter--pulse-characters ["-" "\\" "|" "/"]
  "Characters to use for pulsing progress reporters.")

(defclass progress-reporter-displayer (echo-area-progress-displayer)
  ())

(cl-defmethod progress-displayer-display-progress ((progress-displayer progress-reporter-displayer))
  (let ((progress (progress-displayer-progress progress-displayer)))
    (with-slots (status-message current-step total-steps) progress
      (with-output-to-string
        (let* ((index (mod current-step 4))
               (pulse-char (aref progress-reporter--pulse-characters
                                 index)))
          (princ pulse-char)
          (princ " "))
        (when status-message
          (princ (progress-formatted-status-message progress))
          (princ " "))
        (princ (format "[%d/%d]" current-step total-steps))
        (princ " ")
        (princ (progress-percentage progress))
        (princ "%%")))))

(provide 'progress-reporter)

;;; progress-reporter.el ends here
