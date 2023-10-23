(in-package :megastrike)

(defun end-phase-click (hex hex-units)
  nil)

(defun do-end-phase ()
  (setf (game/units *game*) (remove-if #'cu/destroyedp (game/units *game*)))
  (mapcar #'reset-unit (game/units *game*)))

(defun reset-unit (u)
  (setf (cu/target u) nil
        (cu/move-used u) nil
        (cu/destination u) nil
        (cu/actedp u) nil))
