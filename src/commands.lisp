(in-package :megastrike)

(define-megastrike-command (com-select-unit :name "Select")
  ((selected 'combat-unit))
  (select selected))

(define-presentation-to-command-translator unit-selector
    (combat-unit com-select-unit megastrike :gesture :select :echo nil)
    (object)
  (list object))

(define-megastrike-command (com-select-force :name "Select Force")
  ((selected 'force))
  (setf (game/selected-force *game*) selected))

(define-presentation-to-command-translator force-selector
    (force com-select-force megastrike :gesture :select :echo nil)
    (object)
  (list object))

(define-megastrike-command (com-select-mek :name "Select Mek")
  ((selected 'mek))
  (setf (lobby/selected-mek *lobby*) selected))

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
                             target)))
    (setf (game/phase-log *game*)
          (concatenate 'string (game/phase-log *game) (format nil "Range from ~a to ~a is ~d.~%"
                                                 (offset-from-hex (new-hexagon
                                                                   :q (location/q origin)
                                                                   :r (location/r origin)
                                                                   :s (location/s origin)))
                                                 target
                                                 range)))
    (notify-user *application-frame* range)))

(define-megastrike-command (com-roll-initiative
                 :name "Roll Initiative"
                 :menu t)
  ()
  (setf (game/initiative-list *game*)
        (roll-initiative (game/forces *game*))))

(define-megastrike-command (com-command-move-unit
                 :name "Move"
                 :menu t)
  ((destination 'tile))
  (when (and (eq (game/current-phase *game*) 2) (game/active-unit *game*))
    (move-unit (game/active-unit *game*) destination)))


(define-megastrike-command (com-command-attack
                 :name "Attack"
                 :menu t)
  ((target 'combat-unit))
  (when (and (eq (game/current-phase *game*) 3) (game/active-unit *game*))
    (make-attack (game/active-unit *game*) target)))

(define-megastrike-command (com-roll :name "Roll" :menu t)
  ()
  (let ((roll (format nil "Rolled a ~d on 2D6." (roll2d))))
    (setf (game/phase-log *game*)
          (concatenate 'string (game/phase-log *game*) roll (format nil "~%")))
    (notify-user *application-frame* roll)))

;; TODO Move logic out of command
(define-megastrike-command (com-deploy-unit :name "Deploy" :menu t)
  ((u 'combat-unit)
   (h 'tile))
  (if (not (tile-occupied h))
      (set-location u h)
      (notify-user *application-frame*
                   (format nil "Hex ~2d~2d is occupied."
                           (first (hex-from-offset h))
                           (second (hex-from-offset h))))))

(define-megastrike-command (com-advance-phase
                 :name "Next Phase"
                 :menu t)
  ()
  (do-phase *application-frame*))

(define-megastrike-command (com-quit-game :name "Quit Game" :menu t)
  ()
  (clear-entities)
  (setf (game/forces *game*) '())
  (frame-exit *application-frame*))

(defun main ()
  (mito:connect-toplevel :sqlite3 :database-name ":memory:")
  (mito:ensure-table-exists 'mek)
  (load-data)
  (build-mul)
  (setf *lobby* (new-lobby))
  (setf *game* (lobby/game *lobby*))
  (run-frame-top-level
   (make-application-frame 'megastrike
                           :min-width 800
                           :min-height 800
                           :layout +default-layout+)))
