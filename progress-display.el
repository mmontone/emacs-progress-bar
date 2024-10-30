;;; progress-display.el --- Top-level abstract class for progress display  -*- lexical-binding: t; -*-

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

(defclass progress-display ()
  ((progress :type progress
             :initarg :progress
             :accessor progress-bar-progress)
   (min-time ;;:initform progress-bar-min-time
    :type float
    :initarg :min-time
    :accessor progress-bar-min-time
    :documentation "The minimum time interval between progress bar displays.")
   (min-change ;;:initform progress-bar-min-change
    :initarg :min-change
    :type integer
    :accessor progress-bar-min-change
    :documentation "The minimum percentage change between progress bar displays.")
   (displayed-time :initform 0.0
                   :type float
                   :accessor progress-bar-displayed-time
                   :documentation "Time of last display.")
   (displayed-percentage :initform 0 :type integer
                         :accessor progress-bar-displayed-percentage
                         :documentation "Last percentage displayed.")))

(provide 'progress-display)

;;; progress-display.el ends here
