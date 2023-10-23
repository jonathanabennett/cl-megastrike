(in-package :megastrike)

(defun deployment-phase-click (hex hex-units)
  (when (and (game/active-unit *game*) (not hex-units))
    (deploy (game/active-unit *game*) hex)))

(defmethod deploy ((u combat-unit) (h hexagon))
  (unless (game/tile-occupied-p *game* h)
    (setf (cu/location u) h)))

(defun do-deployment-phase ()
  "This function runs each round to determine whether or not to simply skip the
deployment phase. Deployment phases are only run when there are deployable units."
  (setf (game/phase-log *game*)
        (concatenate 'string (game/phase-log *game*)
                     (format nil "In Deployment Phase~%")))
  (let ((to-deploy '()))
    (mapcar #'(lambda (e) (if (eql (cu/location e) nil) (push e to-deploy))) (game/units *game*))
    (when (= (length to-deploy) 0)
       (advance-phase))))
