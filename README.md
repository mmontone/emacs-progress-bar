# emacs-progress-displayer

A progress displayer for Emacs

![progress-bar](progress-bar.gif)

## Install

`progress.el` has the progress bar implementation.
`progress-displayer.el` has progress displayer implementation.
`progress-tests.el` has tests.
`progress-examples.el` has examples to evaluate.

This is work in progress. To try, load `progress.el`, then `progress-displayer.el`, then follow and evaluate `progress-examples.el`.

## Implementation

It is divided in two, a model layer that consists of `progress` objects, takes care of progress status and triggers update messages. It is unaware of display methods.
A progress display layer, that registers to progress events and displays the progress in some way.
Current implementation features four progress displayers, three that work on the echo area, including a progress bar. And one that shows on the mode line and uses SVG animations. Have a look at `progress-examples.el` for demos.

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


