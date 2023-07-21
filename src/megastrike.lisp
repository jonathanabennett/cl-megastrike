;;;; megastrike.lisp

(in-package #:megastrike)

(defparameter +color-list+ `(("Red" . ,+red+)
                             ("Blue" . ,+blue+)
                             ("Purple" . ,+purple+)
                             ("Green" . ,+light-green+)
                             ("Gold" . ,+gold+)))

(define-application-frame megastrike ()
  ((active-unit
    :initform nil
    :accessor active-unit)
   (armies
    :initarg :armies
    :initform '()
    :accessor frame/armies)
   (selected-army
    :initform nil
    :accessor lobby/selected-army)
   (detail-unit
    :initform nil
    :accessor lobby/detail-unit)
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
      :incremental-redisplay t
      :display-function #'display-overview)
     (lobby-army-list
      :application
      :display-function #'display-lobby-army-list)
     (lobby-detail-view
      :application
      :min-width 375
      :scroll-bars t
      :display-function #'display-lobby-detail-view)
     )
  (:layouts
   (:default
    (vertically ()
      (:fill
       (horizontally ()
         lobby-overview
         (vertically ()
           lobby-detail-view
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
  (format stream "Current Army List~%")
  (formatting-table (stream)
    (formatting-row (stream)
      (formatting-cell (stream)
        (format stream "Army"))
      (formatting-cell (stream)
        (format stream "Color")))
    (dolist (a (frame/armies *application-frame*))
      (formatting-row (stream)
        (formatting-cell (stream)
          (format stream "~a" (army/name a)))
      (formatting-cell (stream)
        (draw-rectangle* stream 0 0 30 30 :ink (army/color a))))))
  (terpri stream)
  (let ((army-text (make-pane 'text-field :width 200))
        (army-color (make-pane 'list-pane
                               :items (mapcar #'car +color-list+))))
    (format stream "Army Name:    ")
    (with-drawing-options (stream :foreground +black+)
      (with-output-as-gadget (stream)
        army-text))
    (terpri stream)
    (surrounding-output-with-border (stream :ink +grey30+ :shape :rounded)
      (with-output-as-gadget (stream)
        army-color))
    (terpri stream)
    (with-output-as-gadget (stream)
      (let ((new-army-button (make-pane 'push-button
                                        :label "New Army"
                                        :activate-callback #'(lambda (gadget)
                                                               (new-army (gadget-value army-text)
                                                                       (cdr (assoc (gadget-value army-color) +color-list+)))
                                                               (redisplay-frame-panes *application-frame*)))))
        new-army-button))))


(defmethod display-lobby-army-list ((frame megastrike) stream)
  (format stream "In Army List pane."))

(defmethod display-lobby-detail-view ((frame megastrike) stream)
  (let ((meks (mito:retrieve-dao 'mek)))
    (formatting-table (stream)
      (formatting-row (stream)
        (formatting-cell (stream) (write-string "Unit name" stream))
        (formatting-cell (stream) (write-string "PV" stream))
        (formatting-cell (stream) (write-string "Size" stream))
        (formatting-cell (stream) (write-string "Move" stream))
        (formatting-cell (stream) (write-string "S/M/L" stream))
        (formatting-cell (stream) (write-string "OV" stream))
        (formatting-cell (stream) (write-string "A/S" stream))
        (formatting-cell (stream) (write-string "Specials" stream)))
    (dolist (m meks)
      (present m 'mek :stream stream)))))

(defmethod display-map ((frame megastrike) stream)
  (maphash (lambda (k v)
             (declare (ignorable k))
             (present v 'tile :stream stream))
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
