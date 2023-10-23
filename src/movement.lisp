(in-package :megastrike)

(defun movement-phase-click (hex hex-units)
  (when (and (game/active-unit *game*) (not hex-units))
    (move-unit (game/active-unit *game*) hex)))

(defun do-movement-phase ()
  "This function will handle the movement phase."
  (setf (game/phase-log *game*)
        (concatenate 'string (game/phase-log *game*)
                     (format nil "In Movement Phase~%")))
  (setf (game/phase-log *game*)
        (concatenate 'string (game/phase-log *game*)
                     "Entering Movement phase.")))
