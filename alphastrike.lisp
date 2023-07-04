;;;; alphastrike.lisp

(in-package #:alphastrike)
(defparameter *here* (asdf:system-source-directory :alphastrike))

(defvar *layout* (make-layout :hex-to-pixel-matrix (vector (/ 3.0 2.0) 0 (/ (sqrt 3.0) 2.0) (sqrt 3.0))
                              :pixel-to-hex-matrix (vector (/ 2.0 3.0) 0 (/ 1.0 3.0) (/ (sqrt 3.0) 3.0))
                              :start-angle 0
                              :x-size 10
                              :y-size 10
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
      (1/6 int)))))

(defvar *test-map* (list (new-hexagon :q -1 :r -1 :s  2)
                         (new-hexagon :q  0 :r -1 :s  1)
                         (new-hexagon :q  1 :r -1 :s  0)
                         (new-hexagon :q -1 :r  0 :s  1)
                         (new-hexagon :q  0 :r  0 :s  0)
                         (new-hexagon :q  1 :r  0 :s -1)
                         (new-hexagon :q -1 :r  1 :s  0)
                         (new-hexagon :q  0 :r  1 :s -1)))

(defmethod display-map ((frame alphastrike) stream)
  (format stream "In the map~%")
  (dolist (loc *test-map*)
    (draw-polygon stream (draw-hex loc *layout*) :filled nil :line-thickness 2)))

(defmethod display-element ((frame alphastrike) stream)
  (let ((pane (get-frame-pane frame 'record-sheet)))
    (with-text-style (stream (make-text-style :serif :bold :large))
      (format stream "~a: ~a~%" (combat-unit-id *locust*) (name (element *locust*)))
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
      )
    (terpri stream)
    (quickstats-block stream *locust*))))


(defun main ()
  (load-data)
  (setf *locust* (make-combat-unit :pilot (make-instance
                                           'pilot
                                           :name "Shooty McShootyface")
                                   :unit (phoenix-hawk-pxh-1d)))
  (run-frame-top-level
   (make-application-frame 'alphastrike
                           :width 800
                           :height 800)))
