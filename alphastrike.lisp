;;;; alphastrike.lisp

(in-package #:alphastrike)
(defparameter *here* (asdf:system-source-directory :alphastrike))

(defvar *layout* (make-layout :hex-to-pixel-matrix (vector (/ 3.0 2.0) 0 (/ (sqrt 3.0) 2.0) (sqrt 3.0))
                              :pixel-to-hex-matrix (vector (/ 2.0 3.0) 0 (/ 1.0 3.0) (/ (sqrt 3.0) 3.0))
                              :start-angle 0
                              :x-size 35
                              :y-size 35
                              :x-origin 10
                              :y-origin 10))

(defun load-data ()
  "Load the contents of the data directory in prepration for execution."
  (uiop:chdir *here*)

  ;; Needs a way to load the full directory
  (load "data/units/locust-lct-1v.lisp")
  (load "data/units/phoenix-hawk-pxh-1d.lisp")
  (load "data/units/marauder-mad-3r.lisp")
  (load "data/units/longbow-lgb-0w.lisp")
  )

(defvar *locust*)

(define-application-frame alphastrike ()
  ()
  (:panes
     (world
      :application
      :display-function #'display-map)
     (record-sheet
      :application
      :display-function #'display-element)
     (int :interactor))
  (:layouts
   (:default
    (vertically ()
      (:fill
       (horizontally ()
         (:fill world)
         (4/9 record-sheet)))
      (1/8 int)))))

(defvar *test-map* (make-instance 'grid))

(defmethod display-map ((frame alphastrike) stream)
  (maphash (lambda (k v)
             (present v 'tile))
           (tiles *test-map*))
  (let ((stream stream))
    (run-draw-units))
  )

(defmethod display-element ((frame alphastrike) stream)
  (let ((pane (get-frame-pane frame 'record-sheet)))
    (with-text-style (stream (make-text-style :serif :bold :large))
      (format stream "~a: ~a~%" (entity-id *locust*) (info/full-name *locust*)))
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
          (format stream "~{~A ~}" (specials/special-list *locust*))))
      (formatting-row (stream)
        (formatting-cell (stream)
          (format stream "Critical Hits: "))
        (formatting-cell (stream)
          (format stream "~{~A ~}" (damageable/crit-list *locust*)))))
    (terpri stream)
    (quickstats-block stream *locust*)))

(define-alphastrike-command (com-damage-unit :name "Damage")
  ()
  (take-damage *locust*))

(define-alphastrike-command (com-reset :name "Reset")
  ()
  (setf (damageable/cur-armor *locust*) (damageable/max-armor *locust*))
  (setf (damageable/cur-struct *locust*) (damageable/max-struct *locust*)))

(define-presentation-to-command-translator tile-selector
    (tile com-inspect-tile alphastrike :gesture :select)
    (object)
  (list object))

(defun main ()
  ;;(load-data)
  (load-board-file #P"data/boards/16x17 Grassland 1.board" *test-map*)
  (load-data)
  (setf *locust* (locust-lct-1v))
  (setf (location/q *locust*) 1)
  (setf (location/r *locust*) 1)
  (setf (damageable/crit-list *locust*) '())
  (setf (pilot/name *locust*) "Shooty McPilotface")
  (setf (pilot/skill *locust*) 4)
  (run-frame-top-level
   (make-application-frame 'alphastrike
                           :width 800
                           :height 800)))
