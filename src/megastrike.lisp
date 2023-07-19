;;;; megastrike.lisp

(in-package #:megastrike)
(defparameter *here* (asdf:system-source-directory :megastrike))
(defparameter *selected-text-style* (make-text-style :serif :bold :normal))

(defparameter +color-list+ '(("Red" +red+)
                             ("Blue" +blue+)
                             ("Purple" +purple+)
                             ("Green" +light-green+)
                             ("Gold" +gold+)))

(define-application-frame megastrike ()
  ((active-unit
    :initform nil
    :accessor active-unit)
   (armies
    :initarg :armies
    :initform '()
    :accessor frame/armies)
   (game-board
    :initarg :game-board
    :initform (make-instance 'grid)
    :accessor frame/game-board)
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
   (layout
    :accessor frame/layout
    :initarg :layout
    :initform (make-layout :hex-to-pixel-matrix (vector (/ 3.0 2.0) 0 (/ (sqrt 3.0) 2.0) (sqrt 3.0))
                           :pixel-to-hex-matrix (vector (/ 2.0 3.0) 0 (/ 1.0 3.0) (/ (sqrt 3.0) 3.0))
                           :start-angle 0
                           :x-size 45
                           :y-size 45
                           :x-origin 10
                           :y-origin 10)))
  (:menu-bar nil)
  (:panes
     (game-world
      :application
      :default-view +graphical-view+
      :display-function #'display-map)
     (game-record-sheet
      :application
      :min-width 375
      :scroll-bars nil
      :display-function #'display-record-sheet)
     (game-quickstats
      :application
      :scroll-bars nil
      :min-width 375
      :default-view +quickstats-view+
      :display-function #'display-quickstats)
     (menu :command-menu)
     (lobby-overview
      :application
      :scroll-bars nil
      :display-function #'display-overview)
     (lobby-army-list
      :application
      :display-function #'display-lobby-army-list)
     (lobby-record-sheet
      :application
      :min-width 375
      :scroll-bars nil
      :display-function #'display-lobby-record-sheet)
     )
  (:layouts
   (:default
    (vertically ()
      (:fill
       (horizontally ()
         lobby-overview
         (vertically ()
           lobby-record-sheet
           lobby-army-list)))
      menu))
   (:game-round
    (vertically ()
      (:fill
       (horizontally ()
         (:fill game-world)
         (vertically ()
           game-record-sheet
           game-quickstats)))
      (1/10 menu)))))

(defmethod display-overview ((frame megastrike) stream)
  (new-army "Draconis Combine" +red+)
  (new-army "Lyran Commonwealth" +blue+)
  (format stream "Current Army List~%")
  (if (frame/armies frame)
      (format stream "~{~a~%~}" (mapcar #'army/name (frame/armies *application-frame*))))
  (with-output-as-gadget (stream)
    (let ((new-army-button (make-pane 'push-button
                                      :label "New Army"
                                      :activate-callback 'new-army-callback)))
      new-army-button))
  )

(defun new-army-callback (button)
  (let (name color)
    (let ((stream (frame-standard-input *application-frame*)))
      (accepting-values (stream :own-window t :label "New Army Dialog")
        (setq name (accept 'string :stream stream :prompt "Army Name"))
        (setq color (accept `((completion ,+color-list+ :value-key cadr)
                              :name-key car)
                            :view `(list-pane-view :visible-items 10)
                            :stream stream
                            :prompt "Color"))
        (format *debug-io* "~a ~a" name color)
        (new-army name color)))))

(defmethod display-lobby-army-list((frame megastrike) stream)
  (format stream "In Army List pane."))

(defmethod display-lobby-record-sheet ((frame megastrike) stream)
  (format stream "In Record sheet pane."))

(defmethod display-map ((frame megastrike) stream)
  (maphash (lambda (k v)
             (declare (ignorable k))
             (present v 'tile))
           (tiles (frame/game-board *application-frame*)))
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
  (load-data)
  (run-frame-top-level
   (make-application-frame 'megastrike
                           :min-width 800
                           :min-height 800)))
