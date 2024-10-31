(require 'progress)

;; Switch between these to change the progress displayer
(setq progress-displayer-class 'minimal-message-progress-displayer)
(setq progress-displayer-class 'progress-bar) ;; load progress-bar.el
(setq progress-displayer-class 'progress-reporter-displayer) ;; load progress-reporter.el
(setq progress-displayer-class nil)

(progress-dotimes (x 30)
    (sit-for 0.4))

(with-progress (pg :status-message "Hello" :total-steps 10)
    (cl-loop for x from 1 to 10
             do
             (progress-incf pg)
             (message "This is a message!!: %s" x)
             (sit-for 0.3)
             ))

(let ((progress-min-steps 5))
  (with-progress (pg :status-message "Hello" :total-steps 4)
      (cl-loop for x from 1 to 4
               do
               (progress-incf pg)
               (message "This is a message!!: %s" x)
               (sit-for 0.3)
               )))

(progress-mapc
 (lambda (x)
   (message (format "Processing %s .." x))
   (sit-for 0.3))
 (cl-loop for i from 1 to 10 collect i))

(progress-mapc
 (lambda (x) (sit-for 0.3))
 (cl-loop for i from 1 to 10 collect i)
 :status-message (lambda (pg)
                   (if (progress-completed-p pg)
                       "All elems processed"
                     (format "Processing %s .." (progress-data pg)))))

(progress-dolist (x (cl-loop for i from 1 to 10 collect i)
                    :status-message "Working ...")
    (sit-for 0.3))

(progn
  (progress-dolist (x (cl-loop for i from 1 to 10 collect i)
                      :status-message (list "Started ..."
                                            (lambda (pb)
                                              (format "Processing %s..." (progress-data pb)))
                                            "Completed!"))
      (sit-for (seq-random-elt '(0.3 0.4 0.5))))
  ;; Do this to observe the completion message when evaluating with eval-last-sexp
  (sit-for 1))


(progress-dolist (x (cl-loop for i from 1 to 10 collect i)
                    :status-message "Working ...")
    (message "Hello %s" x)
  (sit-for 0.3))

(let ((progress-message-display-layout 'newline))
  (progress-dolist (x (cl-loop for i from 1 to 10 collect i)
                      :status-message "Working ...")
      (message "Hello %s" x)
    (sit-for 0.3)))

(progress-dotimes (x 10 :status-message "Working ...")
    (sit-for 0.3))

(progress-dotimes (x 10 :status-message "Working ...")
    (message "Hello %s" x)
  (sit-for 0.3))

(let ((progress-display-after-seconds 1))
  (progress-mapc
   (lambda (x)
     (message (format "Processing %s .." x))
     (sit-for 0.3))
   (cl-loop for i from 1 to 10 collect i)))
