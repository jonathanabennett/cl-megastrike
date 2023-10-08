(in-package :megastrike)

(defun map-click-hander (hex)
  "Determine if there is a unit in the hex clicked on and this pass the hex and the units on to
the appropriate function based on what phase of the game it is."
  (let ((hex-units (game/tile-occupied-p *game* hex))
        (phase (game/current-phase *game*)))
    (cond
      ((= phase 0) (initiative-phase-click hex hex-units))
      ((= phase 1) (deployment-phase-click hex hex-units))
      ((= phase 2) (movement-phase-click hex hex-units))
      ((= phase 3) (combat-phase-click hex hex-units))
      ((= phase 4) (end-phase-click hex hex-units)))))

(defun initiative-phase-click (hex hex-units)
  nil)

(defun deployment-phase-click (hex hex-units)
  (when (game/active-unit *game*)
    (deploy (game/active-unit *game*) hex)))

(defun movement-phase-click (hex hex-units)
  nil)

(defun combat-phase-click (hex hex-units)
  nil)

(defun end-phase-click (hex hex-units)
  nil)

(defun do-phase ()
  "Set appropriate tracking variables, check which phase it is, and dispatch to the
appropriate function."
  (setf (game/initiative-place *game*) 0)
  (setf (game/active-unit *game*) nil)
  (let ((phase (game/current-phase *game*)))
    (cond
      ((= phase 0) (do-initiative-phase))
      ((= phase 1) (do-deployment-phase))
      ((= phase 2) (do-movement-phase))
      ((= phase 3) (do-combat-phase))
      ((= phase 4) (do-end-phase)))))

(defun do-initiative-phase ()
  "This function runs the initiative phase automatically (since the initiative phase
requires no user intervention.)"
  (setf (game/phase-log *game*)
        (concatenate 'string (game/phase-log *game*)
                     (format nil "In Initiative Phase~%")))
  (setf (game/initiative-list *game*)
        (roll-initiative (game/forces *game*))))

(defun roll-initiative (force-list)
  (mapcar #'(lambda (a) (setf (force/initiative a) (roll2d))) force-list)
  (if (eql 2 (length (remove-duplicates force-list :test #'(lambda (a b)
                                                           (eql (force/initiative a)
                                                                (force/initiative b))))))
      (build-initiative-order force-list)
      (roll-initiative force-list)))

(defun build-initiative-order (force-list)
  (let* ((force-order (sort force-list #'(lambda (a b) (< (force/initiative a)
                                                       (force/initiative b)))))
         (turn-order (make-turn-order (turn-order-list (first force-order))
                                      (turn-order-list (second force-order)))))
    (setf (game/phase-log *game*)
          (concatenate 'string (game/phase-log *game*) (format nil "~a~%" turn-order)))
    turn-order))

(defun make-turn-order (losing-force-list winning-force-list)
  (let ((turn-order '()))
    (if (< (length losing-force-list) (length winning-force-list))
        (progn
          (dolist (unit losing-force-list)
            (let ((c (floor (/ (length winning-force-list) (length losing-force-list)))))
              (push (pop losing-force-list) turn-order)
              (dotimes (i c)
                (push (pop winning-force-list) turn-order)))))
        (progn
          (dolist (unit winning-force-list)
            (dotimes (c (floor (/ (length losing-force-list) (length winning-force-list))))
              (format nil "~a" c)
              (push (pop losing-force-list) turn-order))
            (push (pop winning-force-list) turn-order))))
    (reverse turn-order)))

(defun do-deployment-phase ()
  "This function runs each round to determine whether or not to simply skip the
deployment phase. Deployment phases are only run when there are deployable units."
  (setf (game/phase-log *game*)
        (concatenate 'string (game/phase-log *game*)
                     (format nil "In Deployment Phase~%")))
  (let ((to-deploy '()))
     (setf (frame-current-layout frame) :game-round)
     (map-entities #'(lambda (e) (if (eql (location/q e) nil) (push e to-deploy))))
     (when (= (length to-deploy) 0)
       (setf (frame-current-layout frame) :round-report))))

(defun deploy-unit ()
  (let ((force (nth (game/initiative-place *game*) (game/initiative-list *game*)))
        (screen (find-pane-named frame 'game-world)))
    (draw-text screen (format screen "~a's turn to deploy." force) (make-point 5 100))))

(defun do-movement-phase ()
  "This function will handle the movement phase."
  (setf (game/phase-log *game*)
        (concatenate 'string (game/phase-log *game*)
                     (format nil "In Movement Phase~%")))
  (setf (frame-current-layout frame) :game-round)
  (setf (game/phase-log *game*)
        (concatenate 'string (game/phase-log *game*)
                     "Entering Movement phase.")))

(defun do-combat-phase ()
  "This function will handle the combat phase."
  (setf (game/phase-log *game*)
        (concatenate 'string (game/phase-log *game*)
                     (format nil "In Combat Phase~%")))
  (setf (frame-current-layout frame) :game-round)
  (setf (game/phase-log *game*)
        (concatenate 'string (game/phase-log *game*)
                     "Entering Combat phase.")))

(defun do-end-phase ()
  (setf (game/phase-log *game*)
        (concatenate 'string (game/phase-log *game*)
                     (format nil "In End Phase~%")))
  (incf (game/turn-number *game*))
  (run-end-phase))
