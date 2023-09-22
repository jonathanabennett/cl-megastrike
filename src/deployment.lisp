(in-package :megastrike)

;;;; Managing and setting up the Deployment Phase.

(defmethod deploy ((u combat-unit) (h tile))
  (unless (tile-occupied-p h)
    (setf (cu/location u) h)))
