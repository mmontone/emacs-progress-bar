;;; progress-bar-alert.el --- Integration of alert and progress-bar  -*- lexical-binding: t; -*-

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
(require 'progress-bar)

(defun progress-bar-alerter (event progress-bar)
  (case event
    (started
     (alert (format "Started: %s" (progress-bar-formatted-status-message progress-bar))))
    (completed
     (alert (format "Completed: %s" (progress-bar-formatted-status-message progress-bar))))))

(add-hook 'progress-bar-update-functions #'progress-bar-alerter)

;; test
(when nil
  (progress-bar-dotimes (x 5 :status-message "Doing something")
      (sit-for 0.5)))

(provide 'progress-bar-alert)

;;; progress-bar-alert.el ends here
