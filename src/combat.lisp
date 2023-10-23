(in-package :megastrike)

(defun combat-phase-click (hex unit)
  (format t "Combat Phase click Running.")
  (when (and (game/active-unit *game*) unit)
    (format t "Target: ~a" (cu/full-name unit))
    (setf (cu/target (game/active-unit *game*)) unit)))

(defun do-combat-phase ()
  "This function will handle the combat phase."
  (setf (game/phase-log *game*)
        (concatenate 'string (game/phase-log *game*)
                     (format nil "In Combat Phase~%")))
  (setf (game/phase-log *game*)
        (concatenate 'string (game/phase-log *game*)
                     "Entering Combat phase.")))
