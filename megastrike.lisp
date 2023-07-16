;;;; megastrike.lisp

(in-package #:megastrike)
(defparameter *here* (asdf:system-source-directory :megastrike))

(defvar *layout* (make-layout :hex-to-pixel-matrix (vector (/ 3.0 2.0) 0 (/ (sqrt 3.0) 2.0) (sqrt 3.0))
                              :pixel-to-hex-matrix (vector (/ 2.0 3.0) 0 (/ 1.0 3.0) (/ (sqrt 3.0) 3.0))
                              :start-angle 0
                              :x-size 35
                              :y-size 35
                              :x-origin 10
                              :y-origin 10))

(defvar *test-map* (make-instance 'grid))

(define-application-frame megastrike ()
  ((active-unit
    :initform nil
    :accessor active-unit)
   (game-board
    :initform (make-instance 'grid)
    :accessor game-board)
   (current-phase
    :initform 0
    :accessor current-phase)
   (turn-number
    :initform 0
    :accessor turn-number)
   (initiative-list
    :initform '()
    :accessor initiative-list)
   (initiative-place
    :initform 0
    :accessor initiative-place)
   )
  (:menu-bar nil)
  (:panes
     (world
      :application
      :default-view +graphical-view+
      :display-function #'display-map)
     (record-sheet
      :application
      :min-width 375
      :scroll-bars nil
      :display-function #'display-record-sheet)
     (quickstats
      :application
      :scroll-bars nil
      :min-width 375
      :default-view +quickstats-view+
      :display-function #'display-quickstats)
     (menu :command-menu)
     ;;(int :interactor)
     )
  (:layouts
   (:default
    (vertically ()
      (:fill
       (horizontally ()
         (:fill world)
         (vertically ()
           record-sheet
           quickstats)))
      (1/10 menu)))))

(defmethod display-map ((frame megastrike) stream)
  (maphash (lambda (k v)
             (present v 'tile))
           (tiles *test-map*))
  (run-draw-units))

(defmethod display-record-sheet ((frame megastrike) stream)
  (with-text-style (stream (make-text-style :serif :bold :large))
     (format stream "Turn: ~8a Phase: ~a~%"
             (turn-number frame) (nth (current-phase frame) *phase-order*)))
  (if (active-unit frame) (unit-detail stream (active-unit frame)))
  (terpri stream)
  (format stream "~a" (initiative-list *application-frame*)))

(defmethod display-quickstats ((frame megastrike) stream)
  (run-show-quickstats))

(defun main ()
  ;;(load-data)
  (load-board-file (merge-pathnames #P"data/boards/16x17 Grassland 1.board" *here*) *test-map*)
  (load-data)
  (new-army "Draconis Combine" +red+)
  (new-army "Lyran Alliance" +blue+)
  (make-combat-unit 'locust-lct-1v (list 1 1) "Takashi Ujiro" 4 (first *armies*))
  (make-combat-unit 'phoenix-hawk-pxh-1d (list 7 4) "Peter Steele" 4 (second *armies*))
  (make-combat-unit 'marauder-mad-3r (list 2 2) "Sven Stevensen" 4 (first *armies*))
  (make-combat-unit 'longbow-lgb-0w (list 10 14) "Jaime Foxx" 4 (second *armies*))
  (run-frame-top-level
   (make-application-frame 'megastrike
                           :width 800
                           :height 800)))
