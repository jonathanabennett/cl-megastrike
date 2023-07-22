(in-package :megastrike)

(define-megastrike-command (com-select-unit :name "Select")
  ((selected 'combat-unit))
  (if (or (not (initiative-list *application-frame*))
          (< (length (initiative-list *application-frame*)) (initiative-place *application-frame*)))
      (progn(setf (active-unit *application-frame*) selected)
            (map-entities #'(lambda(e) (if (eq (entity-id selected) (entity-id e))
                                           (setf (can-activate/selectedp e) t)
                                           (setf (can-activate/selectedp e) nil)))))
      (if (and (not (can-activate/has-acted selected))
               (same-army (info/army selected) (nth (initiative-place *application-frame*)
                                                    (initiative-list *application-frame*))))
          (progn(setf (active-unit *application-frame*) selected)
                (map-entities #'(lambda(e) (if (eq (entity-id selected) (entity-id e))
                                               (setf (can-activate/selectedp e) t)
                                               (setf (can-activate/selectedp e) nil))))))))

(define-presentation-to-command-translator unit-selector
    (combat-unit com-select-unit megastrike :gesture :select)
    (object)
  (list object))

(define-megastrike-command (com-select-army :name "Select Army")
  ((selected 'army))
  (setf (lobby/selected-army *application-frame*) selected))

(define-presentation-to-command-translator army-selector
    (army com-select-army megastrike :gesture :select)
    (object)
  (list object))

(define-megastrike-command (com-launch-game :name "Launch Game" :menu t)
  ()
  (load-board-file (merge-pathnames #P"data/boards/16x17 Grassland 1.board" *here*)
                   (frame/game-board *application-frame*))
  (let ((dc (new-army "Draconis Combine" +red+))
        (la (new-army "Lyran Alliance" +blue+)))
    (make-combat-unit 'locust-lct-1v (list 1 1) "Takashi Ujiro" 4 dc)
    (make-combat-unit 'marauder-mad-3r (list 2 2) "Sven Stevensen" 4 dc)
    (make-combat-unit 'phoenix-hawk-pxh-1d (list 7 4) "Peter Steele" 4 la)
    (make-combat-unit 'longbow-lgb-0w (list 10 14) "Jaime Foxx" 4 la))
  (setf (frame-current-layout *application-frame*) :game-round))

(define-megastrike-command (com-measure-range
                 :name "Range"
                 :menu t)
  ((origin 'combat-unit)
   (target 'tile))
  (notify-user *application-frame*
               (format nil "Range: ~d" (hex-distance (new-hexagon :q (location/q origin)
                                                                  :r (location/r origin)
                                                                  :s (location/s origin))
                                                     (tile-hexagon target)))))

(define-megastrike-command (com-roll-initiative
                 :name "Roll Initiative"
                 :menu t)
  ()
  (format *debug-io* "Rolling initiative.")
  (setf (initiative-list *application-frame*)
        (roll-initiative (frame/armies *application-frame*))))

(define-megastrike-command (com-command-move-unit
                 :name "Move"
                 :menu t)
  ((destination 'tile))
  (if (and (eq (current-phase *application-frame*) 2) (active-unit *application-frame*))
      (move-unit (active-unit *application-frame*) destination)))


(define-megastrike-command (com-command-attack
                 :name "Attack"
                 :menu t)
  ((target 'combat-unit))
  (if (and (eq (current-phase *application-frame*) 3) (active-unit *application-frame*))
      (make-attack (active-unit *application-frame*) target)))

(define-megastrike-command (com-roll :name "Roll" :menu t)
  ()
  (notify-user *application-frame* (format nil "Rolled a ~d on 2D6." (roll2d))))

(define-megastrike-command (com-advance-phase
                 :name "Next Phase"
                 :menu t)
  ()
  (let ((phase (nth (current-phase *application-frame*) *phase-order*)))
    (cond
       ((eql phase :end) (do-end-phase *application-frame*))
       (t                (do-phase *application-frame*)))))

(define-megastrike-command (com-quit-game :name "Quit Game" :menu t :command-table common-actions)
  ()
  (clear-entities)
  (setf (frame/armies *application-frame*)'())
  (frame-exit *application-frame*))
