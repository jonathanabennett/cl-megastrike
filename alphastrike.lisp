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
    :display-function #'display-element)
   (int :interactor))
  (:layouts
   (default
    (vertically ()
      (1/2 record-sheet)
      (1/2 int)))))

(defmethod display-element ((frame alphastrike) stream)
  (let ((pane (get-frame-pane frame 'record-sheet)))
    (with-text-style (stream (make-text-style :serif :bold :large))
      (format stream "~a~%" (id *locust*)))
    (formatting-table (stream)
      (formatting-row (stream)
        (formatting-cell (stream)
          (general-info-block stream *locust*)))
      (formatting-row (stream)
          (formatting-cell (stream)
            (attack-info-block stream *locust*)))
      (formatting-row (stream)
          (formatting-cell (stream)
            (damage-info-block stream *locust*)))
      (formatting-row (stream)
        (formatting-cell (stream)
          (format stream "Specials: "))
        (formatting-cell (stream)
          (format stream "~{~A ~}" (specials (element *locust*)))))
      (formatting-row (stream)
        (formatting-cell (stream)
          (format stream "Critical Hits: "))
        (formatting-cell (stream)
          (format stream "~{~A ~}" (crits (element *locust*)))))
      )))


(defun main ()
  (load-data)
  (setf *locust* (make-combat-unit :id 1
                                   :pilot (make-instance
                                           'pilot
                                           :name "Shooty McShootyface")
                                   :unit (locust-lct-1v)))
  (run-frame-top-level
   (make-application-frame 'alphastrike)))
