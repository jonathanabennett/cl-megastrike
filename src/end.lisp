(in-package :megastrike)

(defun end-phase-click (hex hex-units)
  nil)

(defun do-end-phase ()
  (setf (game/phase-log *game*)
        (concatenate 'string (game/phase-log *game*)
                     (format nil "In End Phase~%")))
  )
