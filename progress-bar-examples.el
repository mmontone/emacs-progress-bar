(require 'progress-bar)

(dotimes-with-progress-bar (x 50)
    (sit-for 0.1))

(with-progress-bar (pg :status-message "Hello" :total-steps 10)
    (cl-loop for x from 1 to 10
             do
             (progress-bar-incf pg)
             (message "This is a message!!: %s" x)
             (sit-for 0.3)
             ))

(let ((progress-bar-min-steps 5))
  (with-progress-bar (pg :status-message "Hello" :total-steps 4)
      (cl-loop for x from 1 to 4
               do
               (progress-bar-incf pg 1 t)
               (message "This is a message!!: %s" x)
               (sit-for 0.3)
               )))

(mapc-with-progress-bar
 (lambda (x)
   (message (format "Processing %s .." x))
   (sit-for 0.3))
 (cl-loop for i from 1 to 10 collect i))

(mapc-with-progress-bar
 (lambda (x) (sit-for 0.3))
 (cl-loop for i from 1 to 10 collect i)
 :status-message (lambda (pg)
                   (if (progress-bar-completed-p pg)
                       "All elems processed"
                     (format "Processing %s .." (progress-bar-data pg)))))

(dolist-with-progress-bar (x (cl-loop for i from 1 to 10 collect i)
                             :status-message "Working ...")
    (sit-for 0.3))

(progn
  (dolist-with-progress-bar (x (cl-loop for i from 1 to 10 collect i)
                             :status-message (list "Started ..."
                                                   (lambda (pb)
                                                     (format "Processing %s..." (progress-bar-data pb)))
                                                   "Completed!"))
      (sit-for (seq-random-elt '(0.3 0.4 0.5))))
  ;; Do this to observe the completion message when evaluating with eval-last-sexp
  (sit-for 1))
  

(dolist-with-progress-bar (x (cl-loop for i from 1 to 10 collect i)
                             :status-message "Working ...")
    (message "Hello %s" x)
  (sit-for 0.3))

(let ((progress-bar-message-display-layout 'newline))
  (dolist-with-progress-bar (x (cl-loop for i from 1 to 10 collect i)
                               :status-message "Working ...")
      (message "Hello %s" x)
    (sit-for 0.3)))

(dotimes-with-progress-bar (x 10 :status-message "Working ...")
    (sit-for 0.3))

(dotimes-with-progress-bar (x 10 :status-message "Working ...")
    (message "Hello %s" x)
  (sit-for 0.3))

(let ((progress-bar-display-after-seconds 1))
  (mapc-with-progress-bar
   (lambda (x)
     (message (format "Processing %s .." x))
     (sit-for 0.3))
   (cl-loop for i from 1 to 10 collect i)))
