(in-package :megastrike)

(defun movement-phase-click (hex hex-units)
  nil)

(defun do-movement-phase ()
  "This function will handle the movement phase."
  (setf (game/phase-log *game*)
        (concatenate 'string (game/phase-log *game*)
                     (format nil "In Movement Phase~%")))
  (setf (game/phase-log *game*)
        (concatenate 'string (game/phase-log *game*)
                     "Entering Movement phase.")))
