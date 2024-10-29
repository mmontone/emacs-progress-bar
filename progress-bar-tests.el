;;; progress-bar-tests.el --- Tests for progress-bar package  -*- lexical-binding: t; -*-

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

;; Tests for progress-bar package

;;; Code:

(require 'ert)
(require 'progress-bar)

(defun progress-bar-tests-run ()
  (interactive)
  (ert-run-tests-interactively "progress-bar"))

(ert-deftest progress-bar-status-test ()
  (let ((pg (make-progress-bar :total-steps 5)))
    (should (= 0 (progress-bar-current-step pg)))
    (should (not (progress-bar-completed-p pg)))
    (should (= 0 (progress-bar-percentage pg)))
    (progress-bar-incf pg 5)
    (should (= 5 (progress-bar-current-step pg)))
    (should (progress-bar-completed-p pg))
    (should (= 100 (progress-bar-percentage pg)))
    (should-error (progress-bar-incf pg))))

(ert-deftest dolist-with-progress-bar-test ()
  (let ((l '()))
    (dolist-with-progress-bar (x (list))
        (push x l)
      (sit-for 0.2))
    (should (zerop (length l))))
  (let ((l '()))
    (dolist-with-progress-bar (x (list 1 2 3))
        (push x l)
      (sit-for 0.2))
    (should (equalp l (list 3 2 1)))))

(ert-deftest dotimes-with-progress-bar-test ()
  (let ((l '()))
    (dotimes-with-progress-bar (x 0)
        (push x l)
      (sit-for 0.2))
    (should (zerop (length l))))
  (let ((l '()))
    (dotimes-with-progress-bar (x 5)
        (push x l)
      (sit-for 0.2))
    (should (equalp l (list 4 3 2 1 0)))))

(ert-deftest mapc-with-progress-bar-test ()
  (let ((l '()))
    (mapc-with-progress-bar
     (lambda (x)
       (push x l)
       (sit-for 0.2))
     (list))
    (should (zerop (length l))))
  (let ((l '()))
    (mapc-with-progress-bar
     (lambda (x)
       (push x l)
       (sit-for 0.2))
     (list 1 2 3))
    (should (equalp l (list 3 2 1)))))

(ert-deftest progress-bar-update-functions-test ()
  (let ((events '()))
    (cl-flet ((progress-bar-updated (event _pg)
                (push event events)))
      (let ((progress-bar-update-functions (list #'progress-bar-updated)))
        (dotimes-with-progress-bar (x 5)
            (sit-for 0.2)))
      (should (equalp events '(completed updated updated updated updated updated started))))))

(ert-deftest progress-bar-status-message-test ()
  (let ((pg (make-progress-bar :total-steps 5
                               :status-message "Working ...")))
    (dotimes-with-progress-bar (x 5 pg)
        (should (string= (progress-bar-formatted-status-message pg) "Working ..."))))
  (let ((pg (make-progress-bar :total-steps 5
                               :status-message (lambda (pg) (format "Processing: %s" (progress-bar-data pg))))))
    (dotimes-with-progress-bar (x 5 pg)
        (should (string= (progress-bar-formatted-status-message pg)
                         (format "Processing: %d" x)))))
  (let ((pg (make-progress-bar :total-steps 5
                               :status-message (list "Started"
                                                     (lambda (pg) (format "Processing: %s" (progress-bar-data pg)))
                                                     "Completed"))))
    (dotimes-with-progress-bar (x 5 pg)
        (should (string= (progress-bar-formatted-status-message pg)
                         (format "Processing: %d" x))))
    (should (string= (progress-bar-formatted-status-message pg) "Completed"))))

(provide 'progress-bar-tests)

;;; progress-bar-tests.el ends here
