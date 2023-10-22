(in-package :megastrike)

(defun combat-phase-click (hex hex-units)
  (format nil "Combat Phase click Running.")
  (when (and (game/active-unit *game*) hex-units)
    (format nil "Target: ~a" (cu/full-name (car hex-units)))
    (setf (cu/target (game/active-unit *game*)) (car hex-units))))

(defun do-combat-phase ()
  "This function will handle the combat phase."
  (setf (game/phase-log *game*)
        (concatenate 'string (game/phase-log *game*)
                     (format nil "In Combat Phase~%")))
  (setf (game/phase-log *game*)
        (concatenate 'string (game/phase-log *game*)
                     "Entering Combat phase.")))
