;;; progress-alert.el --- Integration of alert and progress  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Mariano Montone

;; Author: Mariano Montone <marianomontone@gmail.com>
;; Keywords: convenience, tools

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

;; Integration of alert and progress bar

;;; Code:

(require 'alert)
(require 'progress)

(defun progress-alerter (event progress)
  (case event
    (started
     (alert (format "Started: %s" (progress-formatted-status-message progress))))
    (completed
     (alert (format "Completed: %s" (progress-formatted-status-message progress))))))

(add-hook 'progress-update-functions #'progress-alerter)

;; test
(when nil
  (progress-dotimes (x 5 :status-message "Doing something")
      (sit-for 0.5)))

(provide 'progress-alert)

;;; progress-alert.el ends here
