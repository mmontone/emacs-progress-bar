;;; progress-tests.el --- Tests for progress package  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Mariano Montone

;; Author: Mariano Montone <marianomontone@gmail.com>
;; Keywords: convenience

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

;; Tests for progress package

;;; Code:

(require 'ert)
(require 'progress)

(defun progress-tests-run ()
  (interactive)
  (ert-run-tests-interactively "progress"))

(ert-deftest progress-status-test ()
  (let ((pg (make-progress :total-steps 5)))
    (should (= 0 (progress-current-step pg)))
    (should (not (progress-completed-p pg)))
    (should (= 0 (progress-percentage pg)))
    (progress-incf pg 5)
    (should (= 5 (progress-current-step pg)))
    (should (progress-completed-p pg))
    (should (= 100 (progress-percentage pg)))
    (should-error (progress-incf pg))))

(ert-deftest progress-dolist-test ()
  (let ((l '()))
    (progress-dolist (x (list))
        (push x l)
      (sit-for 0.2))
    (should (zerop (length l))))
  (let ((l '()))
    (progress-dolist (x (list 1 2 3))
        (push x l)
      (sit-for 0.2))
    (should (equalp l (list 3 2 1)))))

(ert-deftest progress-dotimes-test ()
  (let ((l '()))
    (progress-dotimes (x 0)
        (push x l)
      (sit-for 0.2))
    (should (zerop (length l))))
  (let ((l '()))
    (progress-dotimes (x 5)
        (push x l)
      (sit-for 0.2))
    (should (equalp l (list 4 3 2 1 0)))))

(ert-deftest progress-mapc-test ()
  (let ((l '()))
    (progress-mapc
     (lambda (x)
       (push x l)
       (sit-for 0.2))
     (list))
    (should (zerop (length l))))
  (let ((l '()))
    (progress-mapc
     (lambda (x)
       (push x l)
       (sit-for 0.2))
     (list 1 2 3))
    (should (equalp l (list 3 2 1)))))

(ert-deftest progress-update-functions-test ()
  (let ((events '()))
    (cl-flet ((progress-updated (event _pg)
                (push event events)))
      (let ((progress-update-functions (list #'progress-updated)))
        (progress-dotimes (x 5)
            (sit-for 0.2)))
      (should (equalp events '(completed updated updated updated updated updated started))))))

(ert-deftest progress-status-message-test ()
  (let ((pg (make-progress :total-steps 5
                           :status-message "Working ...")))
    (progress-dotimes (x 5 pg)
        (should (string= (progress-formatted-status-message pg) "Working ..."))))
  (let ((pg (make-progress :total-steps 5
                           :status-message (lambda (pg) (format "Processing: %s" (progress-data pg))))))
    (progress-dotimes (x 5 pg)
        (should (string= (progress-formatted-status-message pg)
                         (format "Processing: %d" x)))))
  (let ((pg (make-progress :total-steps 5
                           :status-message (list "Started"
                                                 (lambda (pg) (format "Processing: %s" (progress-data pg)))
                                                 "Completed"))))
    (progress-dotimes (x 5 pg)
        (should (string= (progress-formatted-status-message pg)
                         (format "Processing: %d" x))))
    (should (string= (progress-formatted-status-message pg) "Completed"))))

(provide 'progress-tests)

;;; progress-tests.el ends here
