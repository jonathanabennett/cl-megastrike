(in-package :megastrike)

(defun do-combat-phase ()
  "This function will handle the combat phase."
  (setf (game/phase-log *game*)
        (concatenate 'string (game/phase-log *game*)
                     (format nil "In Combat Phase~%")))
  (setf (game/phase-log *game*)
        (concatenate 'string (game/phase-log *game*)
                     "Entering Combat phase.")))
