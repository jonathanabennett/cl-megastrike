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

(defvar *test-map* (make-instance 'grid))

(defun load-data ()
  "Load the contents of the data directory in prepration for execution."
  (uiop:chdir *here*)

  (let ((mech-files (uiop:directory-files (uiop:merge-pathnames* #p"data/units/" *here*))))
    (dolist (file mech-files)
      (if (string= (pathname-type file) "lisp") (load file)))))

(define-application-frame alphastrike ()
  ((active-unit
    :initform nil
    :accessor active-unit)
   (current-phase
    :initform 0
    :accessor current-phase)
   (turn-number
    :initform 0
    :accessor turn-number)
   (initiative-list
    :initform '()
    :accessor initiative-list)
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
      :display-function #'display-element)
     (quickstats
      :application
      :scroll-bars nil
      :min-width 375
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

(defmethod display-map ((frame alphastrike) stream)
  (maphash (lambda (k v)
             (present v 'tile))
           (tiles *test-map*))
  (dolist (a *armies*)
    (draw-units stream a))
  )

(defmethod display-element ((frame alphastrike) stream)
  (with-text-style (stream (make-text-style :serif :bold :large))
     (format stream "Turn: ~8a Phase: ~a~%"
             (turn-number frame) (nth (current-phase frame) *phase-order*)))
  (run-show-unit-stats)
  (terpri stream)
  (format stream "~a" (initiative-list *application-frame*)))

(defmethod display-quickstats ((frame alphastrike) stream)
  (run-show-quickstats))

(define-alphastrike-command (com-damage-unit :name "Damage")
  ((target 'combat-unit
           :prompt "Target: ")
   (damage 'integer
           :default 1
           :prompt "Amount: "))
  (take-damage target damage))

(define-alphastrike-command (com-reset :name "Reset" :menu t)
  ((target 'combat-unit
           :prompt "Target: "))
  (setf (damageable/cur-armor target) (damageable/max-armor target))
  (setf (damageable/cur-struct target) (damageable/max-struct target)))

(define-alphastrike-command (com-roll-initiative :name "Roll Initiative" :menu t)
  ()
  (format *debug-io* "Rolling initiative.")
  (setf (initiative-list *application-frame*) (roll-initiative *armies*)))

(define-alphastrike-command (com-measure-range :name "Measure Range" :menu t)
  ((origin 'combat-unit)
   (target 'tile))
  (notify-user *application-frame*
               (format nil "Range: ~d" (hex-distance (new-hexagon :q (location/q origin)
                                                                  :r (location/r origin)
                                                                  :s (location/s origin))
                                                     (tile-hexagon target)))))

(define-alphastrike-command (com-command-move-unit :name "Move" :menu t)
  ((unit 'combat-unit)
   (destination 'tile))
  (move-unit unit destination))

(define-alphastrike-command (com-command-attack :name "Attack" :menu t)
  ((attacker 'combat-unit)
   (target 'combat-unit))
  (make-attack attacker target))

(define-alphastrike-command (com-roll :name "Roll" :menu t)
  ()
  (notify-user *application-frame* (format nil "Rolled a ~d on 2D6." (roll2d))))

(define-alphastrike-command (com-select-unit :name "Select")
  ((selected 'combat-unit))
  (map-entities #'(lambda(e) (if (eq (entity-id selected) (entity-id e))
                                 (setf (can-activate/selectedp e) t)
                                 (setf (can-activate/selectedp e) nil)))))

(define-alphastrike-command (com-advance-phase :name "Next Phase" :menu t)
  ()
  (let ((phase (nth (current-phase *application-frame*) *phase-order*)))
    (cond
       ((eql phase :end) (do-end-phase *application-frame*))
       (t                (do-phase *application-frame*)))))

(define-alphastrike-command (com-quit-game :name "Quit Game" :menu t)
  ()
  (clear-entities)
  (setf *armies* '())
  (frame-exit *application-frame*))

;; (define-presentation-to-command-translator tile-selector
;;     (tile com-inspect-tile alphastrike :gesture :select)
;;     (object)
;;   (list object))

(define-presentation-to-command-translator unit-selector
    (combat-unit com-select-unit alphastrike :gesture :select)
    (object)
  (list object))


(defun main ()
  ;;(load-data)
  (load-board-file #P"data/boards/16x17 Grassland 1.board" *test-map*)
  (load-data)
  (new-army "Draconis Combine" +red+)
  (new-army "Lyran Alliance" +blue+)
  (let ((mek (locust-lct-1v)))
    (setf (location/q mek) 1)
    (setf (location/r mek) 1)
    (setf (location/s mek) -2)
    (setf (damageable/crit-list mek) '())
    (setf (pilot/name mek) "Shooty McPilotface")
    (setf (pilot/skill mek) 4)
    (add-unit (first *armies*) mek))
  (let ((mek (phoenix-hawk-pxh-1d)))
    (setf (location/q mek) 8)
    (setf (location/r mek) 5)
    (setf (location/s mek) -13)
    (setf (damageable/crit-list mek) '())
    (setf (pilot/name mek) "Drivey McShooterface")
    (setf (pilot/skill mek) 0)
    (add-unit (second *armies*) mek))
  (let ((mek (longbow-lgb-0w)))
    (setf (location/q mek) 8)
    (setf (location/r mek) 7)
    (setf (location/s mek) -15)
    (setf (damageable/crit-list mek) '())
    (setf (pilot/name mek) "")
    (setf (pilot/skill mek) 0)
    (add-unit (second *armies*) mek))
  (run-frame-top-level
   (make-application-frame 'alphastrike
                           :width 800
                           :height 800)))
