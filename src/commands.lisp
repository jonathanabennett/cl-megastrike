(in-package :megastrike)

(define-megastrike-command (com-select-unit :name "Select")
  ((selected 'combat-unit))
  (let ((app *application-frame*))
    (if (or (not (initiative-list app))
            (< (length (initiative-list app)) (initiative-place app)))
        (progn
          (setf (active-unit app) selected)
          (map-entities #'(lambda(e) (if (eq (entity-id selected) (entity-id e))
                                         (setf (can-activate/selectedp e) t)
                                         (setf (can-activate/selectedp e) nil)))))
        (if (and (not (can-activate/has-acted selected))
                 (same-army (info/army selected) (nth (initiative-place app)
                                                      (initiative-list app))))
            (progn
              (setf (active-unit app) selected)
              (map-entities #'(lambda(e) (if (eq (entity-id selected) (entity-id e))
                                             (setf (can-activate/selectedp e) t)
                                             (setf (can-activate/selectedp e) nil)))))))))

(define-presentation-to-command-translator unit-selector
    (combat-unit com-select-unit megastrike :gesture :select :echo nil)
    (object)
  (list object))

(define-megastrike-command (com-select-army :name "Select Army")
  ((selected 'army))
  (setf (lobby/selected-army *application-frame*) selected))

(define-presentation-to-command-translator army-selector
    (army com-select-army megastrike :gesture :select :echo nil)
    (object)
  (list object))

(define-megastrike-command (com-select-mek :name "Select Mek")
  ((selected 'mek))
  (setf (lobby/selected-mek *application-frame*) selected))

(define-presentation-to-command-translator mek-selector
    (mek com-select-mek megastrike :gesture :select :echo nil)
    (object)
  (list object))

(define-megastrike-command (com-measure-range :name "Range" :menu t)
  ((origin 'combat-unit)
   (target 'tile))
  (let ((range (hex-distance (new-hexagon :q (location/q origin)
                                          :r (location/r origin)
                                          :s (location/s origin))
                             (tile-hexagon target))))
    (setf (phase-log *application-frame*)
          (concatenate 'string phase-log (format nil "Range from ~a to ~a is ~d.~%"
                                                 (offset-from-hex (new-hexagon
                                                                   :q (location/q origin)
                                                                   :r (location/r origin)
                                                                   :s (location/s origin)))
                                                 (tile-hexagon target)
                                                 range)))
    (notify-user *application-frame* range)))

(define-megastrike-command (com-roll-initiative
                 :name "Roll Initiative"
                 :menu t)
  ()
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
  (let ((roll (format nil "Rolled a ~d on 2D6." (roll2d))))
    (setf (phase-log *application-frame*)
          (concatenate 'string (phase-log *application-frame*) roll (format nil "~%")))
    (notify-user *application-frame* roll)))

(define-megastrike-command (com-advance-phase
                 :name "Next Phase"
                 :menu t)
  ()
  (let ((phase (nth (current-phase *application-frame*) *phase-order*)))
    (cond
       ((eql phase :end) (do-end-phase *application-frame*))
       (t                (do-phase *application-frame*)))))

(define-megastrike-command (com-quit-game :name "Quit Game" :menu t)
  ()
  (clear-entities)
  (setf (frame/armies *application-frame*)'())
  (frame-exit *application-frame*))

(defun main ()
  (mito:connect-toplevel :sqlite3 :database-name ":memory:")
  (sleep 1)
  (mito:ensure-table-exists 'mek)
  (sleep 1)
  (load-data)
  (sleep 1)
  (build-mul)
  (run-frame-top-level
   (make-application-frame 'megastrike
                           :min-width 800
                           :min-height 800
                           :layout +default-layout+)))
