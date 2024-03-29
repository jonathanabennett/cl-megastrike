(in-package :megastrike)

(defun map-click-handler (hex)
  "Determine if there is a unit in the hex clicked on and this pass the hex and the units on to
the appropriate function based on what phase of the game it is."
  (let ((unit (first (member hex (game/units *game*) :key #'cu/location :test #'same-hex)))
        (phase (game/current-phase *game*)))
    (format t "Click during phase ~a" phase)
    (cond
      ((= phase 0) (initiative-phase-click hex unit))
      ((= phase 1) (deployment-phase-click hex unit))
      ((= phase 2) (movement-phase-click hex unit))
      ((= phase 3) (combat-phase-click hex unit))
      ((= phase 4) (end-phase-click hex unit)))))

(defun advance-phase ()
  (incf (game/current-phase *game*))
  (when (= (game/current-phase *game*) 5)
    (setf (game/current-phase *game*) 0)
    (incf (game/turn-number *game*)))
  (do-phase))

(defun do-phase ()
  "Set appropriate tracking variables, check which phase it is, and dispatch to the
appropriate function."
  (setf (game/initiative-place *game*) 0)
  (setf (game/active-unit *game*) nil)
  (mapcar (lambda (u) (setf (cu/actedp u) nil)) (game/units *game*))
  (let ((phase (game/current-phase *game*)))
    (cond
      ((= phase 0) (do-initiative-phase))
      ((= phase 1) (do-deployment-phase))
      ((= phase 2) (do-movement-phase))
      ((= phase 3) (do-combat-phase))
      ((= phase 4) (do-end-phase)))))
