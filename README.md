# emacs-progress-displayer

A progress displayer for Emacs

![progress-bar](progress-bar.gif)

## Install

`progress.el` has the progress bar implementation.

Load `progress-integrations.el` for integrating the progress bar in some of Emacs operations, like package management.

## Usage

The preferred method for using a progress is via the utility functions:
`progress-dolist`, `dotimes-with-progress` and `mapc-with-progress`.

Example:

```lisp
(progress-dolist (x (cl-loop for i from 1 to 10 collect i)
                              :status-message (list "Started ..."
                                                    (lambda (pb)
                                                      (format "Processing %s..." (progress-data pb)))
                                                    "Completed!"))
     (sit-for (seq-random-elt '(0.3 0.4 0.5))))
```


