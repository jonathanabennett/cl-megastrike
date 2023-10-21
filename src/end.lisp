(in-package :megastrike)

(defun do-end-phase ()
  (setf (game/phase-log *game*)
        (concatenate 'string (game/phase-log *game*)
                     (format nil "In End Phase~%")))
  )
