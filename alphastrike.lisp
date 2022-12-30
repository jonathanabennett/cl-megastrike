;;;; alphastrike.lisp

(in-package #:alphastrike)
(defparameter *here* (asdf:system-source-directory :alphastrike))

(defun load-data ()
  "Load the contents of the data directory in prepration for execution."
  (uiop:chdir *here*)
  (load "data/units/locust-lct-1v.lisp")
  (load "data/units/phoenix-hawk-pxh-1d.lisp")
  (load "data/units/marauder-mad-3r.lisp")
  (load "data/units/longbow-lgb-0w.lisp")
  )

(defvar *locust*)

(define-application-frame alphastrike ()
  ()
  (:panes
   (record-sheet
    :application
    :display-time t
    :display-function #'display-element)
   (int :interactor))
  (:layouts
   (default
    (vertically ()
      (1/2 record-sheet)
      (1/2 int)))))

(defmethod display-element ((frame alphastrike) stream)
  (format stream "~a" (name *locust*)))

(defun main ()
  (load-data)
  (setf *locust* (locust-lct-1v))
  (run-frame-top-level
   (make-application-frame 'alphastrike)))
