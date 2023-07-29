(in-package :megastrike)

(define-system draw-units ((entity location))
  (let ((frame (find-pane-named *application-frame* 'game-world)))
    (if (location/q entity)
        (present entity 'combat-unit :stream frame))))

(define-system show-quickstats ((entity))
  (let ((stream (find-pane-named *application-frame* 'game-quickstats)))
    (present entity 'combat-unit :stream stream )
    (terpri stream)))

(define-system end-phase ((entity))
  (if (damageable/destroyedp entity)
      (destroy-entity entity)))

(define-system advance-phase ((entity))
  (setf (can-activate/selectedp entity) nil)
  (setf (can-activate/has-acted entity) nil))

(define-system list-force ((entity))
  (let ((stream (find-pane-named *application-frame* 'lobby-force-list)))
    (if (same-force (info/force entity) (game/selected-force *game*))
        (formatting-row (stream)
          (formatting-cell (stream) (write-string (info/full-name entity) stream))
          (formatting-cell (stream) (format stream "~a" (info/pv entity)))
          (formatting-cell (stream) (write-string (pilot/name entity) stream))
          (formatting-cell (stream) (format stream "~a" (pilot/skill entity)))))))

(define-system mark-occupied-hexes ((entity location))
  (let ((grid (game/board *game*)))
    ()))

(define-system clear-selection ((e can-activate))
  (setf (can-activate/selectedp e) nil))
