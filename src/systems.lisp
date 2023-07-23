(in-package :megastrike)

(define-system draw-units ((entity location))
  (let ((frame (find-pane-named *application-frame* 'game-world)))
    (present entity 'combat-unit :stream frame)))

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

(define-system list-army ((entity))
  (let ((stream (find-pane-named *application-frame* 'lobby-army-list))
        (hex (new-hexagon
              :q (location/q entity)
              :r (location/r entity)
              :s (location/s entity))))
    (if (same-army (info/army entity) (lobby/selected-army *application-frame*))
        (formatting-row (stream)
          (formatting-cell (stream) (write-string (info/full-name entity) stream))
          (formatting-cell (stream) (format stream "~a" (info/pv entity)))
          (formatting-cell (stream) (write-string (pilot/name entity) stream))
          (formatting-cell (stream) (format stream "~a" (pilot/skill entity)))
          (formatting-cell (stream) (format stream "~2d~2d" (first (offset-from-hex hex))
                                                            (second (offset-from-hex hex))))))))
