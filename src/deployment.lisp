(in-package :megastrike)

;;;; Managing and setting up the Deployment Phase.

(defmethod deploy ((u combat-unit) (h tile))
  (unless (game/tile-occupied-p *game* h)
    (setf (cu/location u) h)))
