;;; progress-integrations.el --- Integrations of progress into Emacs.  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Free Software Foundation, Inc.

;; Author: Mariano Montone <marianomontone@gmail.com>
;; Keywords: tools, convenience, extensions
;; Version: 0.4
;; Package-Requires: ((emacs "27.1") (progress "0.5"))

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

;; Integrates progress into Emacs.
;;
;; As of this moment, this package overwrites some of the package.el operations
;; in order to display a progress bar for them; and makes Emacs progress-reporter
;; work with progresss too.

;;; Code:

(require 'progress)
(require 'package)

;; Packages

(define-advice package-upgrade-all (:override (&optional query))
  "Refresh package list and upgrade all packages.
If QUERY, ask the user before upgrading packages.  When called
interactively, QUERY is always true.

Currently, packages which are part of the Emacs distribution are
not upgraded by this command.  To enable upgrading such a package
using this command, first upgrade the package to a newer version
from ELPA by either using `\\[package-upgrade]' or
`\\<package-menu-mode-map>\\[package-menu-mark-install]' after `\\[list-packages]'."
  (interactive (list (not noninteractive)))
  (package-refresh-contents)
  (let ((upgradeable (package--upgradeable-packages)))
    (if (not upgradeable)
        (message "No packages to upgrade")
      (when (and query
                 (not (yes-or-no-p
                       (if (length= upgradeable 1)
                           "One package to upgrade.  Do it? "
                         (format "%s packages to upgrade.  Do it?"
                                 (length upgradeable))))))
        (user-error "Upgrade aborted"))
      (progress-dolist (package upgradeable
                                :status-message
                                (list "Upgrading packages"
                                      (lambda (pb)
                                        (format "Upgrading package: %s"
                                                (package-desc-name (progress-data pb))))
                                      (format "%d packages upgraded." (length upgradeable))))
          (package-upgrade package)))))

(define-advice package-menu--perform-transaction (:override (install-list delete-list))
  "Install packages in INSTALL-LIST and delete DELETE-LIST.
Return nil if there were no errors; non-nil otherwise."
  (let ((errors nil))
    (if install-list
        (let ((status-format (format ":Installing %%d/%d"
                                     (length install-list)))
              (i 0)
              (package-menu--transaction-status))
          (progress-dolist (pkg install-list
                                :status-message (list (format "Installing %d packages" (length install-list))
                                                      (lambda (pb) (format "Installing package: %s" (package-desc-name (progress-data pb))))
                                                      (format "%d packages installed." (length install-list))))
              (setq package-menu--transaction-status
                    (format status-format (cl-incf i)))
            (force-mode-line-update)
            (redisplay 'force)
            ;; Don't mark as selected, `package-menu-execute' already
            ;; does that.
            (package-install pkg 'dont-select))))
    (let ((package-menu--transaction-status ":Deleting"))
      (force-mode-line-update)
      (redisplay 'force)
      (progress-dolist (elt (package--sort-by-dependence delete-list)
                            :status-message
                            (list (format "Deleting %d packages" (length delete-list))
                                  (lambda (pb)
                                    (format "Deleting package: %s" (package-desc-name (progress-data pb))))
                                  (format "%d packages deleted." (length delete-list))))
          (condition-case-unless-debug err
              (let ((inhibit-message (or inhibit-message package-menu-async)))
                (package-delete elt nil 'nosave))
            (error
             (push (package-desc-full-name elt) errors)
             (message "Error trying to delete `%s': %S"
                      (package-desc-full-name elt) err)))))
    errors))

(define-advice package--download-and-read-archives (:override (&optional async))
  (progress-dolist (archive package-archives
                            :status-message (list (format "Refreshing %d package archives" (length package-archives))
                                                  (lambda (pb) (format "Reading archive: %s" (car (progress-data pb))))
                                                  "Package contents refreshed"))
      (condition-case-unless-debug nil
          (package--download-one-archive archive "archive-contents" async)
        (error (message "Failed to download `%s' archive."
                        (car archive))))))

;; Progress reporter

(defvar progress-reporter-progresses
  (make-hash-table :weakness 'key)
  "A map of PROGRESS-REPORTER instances pointing to PROGRESS instances.")

(defun progress-reporter->progress (reporter value)
  "Convert progress REPORTER and current VALUE to a `progress'.
If a `progress' has already been created, then update its `current-step' and return it."
  (let* ((parameters   (cdr reporter))
         ;;(update-time  (aref parameters 0))
         (min-value    (aref parameters 1))
         (max-value    (aref parameters 2))
         (text         (aref parameters 3)))
    (let ((progress (or (gethash reporter progress-reporter-progresses)
                        (let ((progress (make-progress
                                         :total-steps (- max-value min-value)
                                         :status-message text)))
                          (puthash reporter progress
                                   progress-reporter-progresses)
                          (progress-notify 'started progress)
                          progress))))
      (with-slots (current-step) progress
        (setf current-step (- value min-value)))
      progress)))

(defcustom progress-replace-progress-reporter t
  "When enabled, use a progress bar instead of default Emacs progress reporter."
  :type 'boolean
  :group 'progress)

(defun progress-around-reporter (orig reporter value &optional suffix)
  (if (not progress-replace-progress-reporter)
      (funcall orig reporter value suffix)
    (let* ((parameters   (cdr reporter))
           (min-value    (aref parameters 1))
           (max-value    (aref parameters 2)))
      (if (and (not min-value) (not max-value))
          (funcall orig reporter value suffix)
        ;; TODO: fixme
        ;;(progress-update (progress-reporter->progress reporter)
        ;;                 'current-step value)
        (progress-incf (progress-reporter->progress reporter value))
        ))))

(defun progress-reporter-done-override (reporter)
  (progress-notify 'completed (progress-reporter->progress reporter 0)))

(advice-add 'progress-reporter-do-update :around 'progress-around-reporter)
(advice-add 'progress-reporter-done :override 'progress-reporter-done-override)

(when nil
  (let ((progress-reporter
         (make-progress-reporter "Collecting mana for Emacs..."
                                 0  500)))
    (dotimes (k 500)
      (sit-for 0.01)
      (progress-reporter-update progress-reporter k))
    (progress-reporter-done progress-reporter)))

(provide 'progress-integrations)

;;; progress-integrations.el ends here
