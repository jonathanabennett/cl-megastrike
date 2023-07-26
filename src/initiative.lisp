(in-package :megastrike)

(defun do-phase (frame)
  "Set appropriate tracking variables, check which phase it is, and dispatch to the
appropriate function."
  (setf (initiative-place frame) 0)
  (setf (active-unit frame) nil)
  (run-advance-phase)
  (setf (frame-current-layout frame) :round-report)
  (let ((phase (current-phase frame)))
    (format *debug-io* "Phase: ~a~%" phase)
    (cond
      ((= phase 0) (do-initiative-phase frame))
      ((= phase 1) (do-deployment-phase frame))
      ((= phase 2) (do-movement-phase   frame))
      ((= phase 3) (do-combat-phase     frame))
      ((= phase 4) (do-end-phase        frame)))))

(defun do-initiative-phase (frame)
  "This function runs the initiative phase automatically (since the initiative phase
requires no user intervention.)"
  (setf (phase-log *application-frame*)
        (concatenate 'string (phase-log *application-frame*)
                     (format *debug-io* "In Initiative Phase~%")))
  (setf (initiative-list frame)
        (roll-initiative (frame/armies frame)))
  (setf (frame-current-layout frame) :round-report))

(defun roll-initiative (army-list)
  (mapcar #'(lambda (a) (setf (army/initiative a) (roll2d))) army-list)
  (if (eql 2 (length (remove-duplicates army-list :test #'(lambda (a b)
                                                           (eql (army/initiative a)
                                                                (army/initiative b))))))
      (build-initiative-order army-list)
      (roll-initiative army-list)))

(defun build-initiative-order (army-list)
  (let* ((army-order (sort army-list #'(lambda (a b) (< (army/initiative a)
                                                       (army/initiative b)))))
         (turn-order (make-turn-order (turn-order-list (first army-order))
                                      (turn-order-list (second army-order)))))
    (setf (phase-log *application-frame*)
          (concatenate 'string (phase-log *application-frame*) (format nil "~a~%" turn-order)))
    turn-order))

(defun make-turn-order (losing-army-list winning-army-list)
  (let ((turn-order '()))
    (if (< (length losing-army-list) (length winning-army-list))
        (progn
          (dolist (unit losing-army-list)
            (let ((c (floor (/ (length winning-army-list) (length losing-army-list)))))
              (push (pop losing-army-list) turn-order)
              (dotimes (i c)
                (push (pop winning-army-list) turn-order)))))
        (progn
          (dolist (unit winning-army-list)
            (dotimes (c (floor (/ (length losing-army-list) (length winning-army-list))))
              (format nil "~a" c)
              (push (pop losing-army-list) turn-order))
            (push (pop winning-army-list) turn-order))))
    (reverse turn-order)))

(defun do-deployment-phase (frame)
  "This function runs each round to determine whether or not to simply skip the
deployment phase. Deployment phases are only run when there are deployable units."
  (setf (phase-log *application-frame*)
        (concatenate 'string (phase-log *application-frame*)
                     (format *debug-io* "In Deployment Phase~%")))
  (let ((to-deploy '()))
     (setf (frame-current-layout frame) :game-round)
     (map-entities #'(lambda (e) (if (eql (location/q e) nil) (push e to-deploy))))
     (if (= (length to-deploy) 0)
         (setf (frame-current-layout frame) :round-report)
         (format *debug-io* "Units to deploy~%"))))

(defun deploy-unit (frame)
  (let ((army (nth (initiative-place frame) (initiative-list frame)))
        (screen (find-pane-named frame 'game-world)))
    (draw-text screen (format screen "~a's turn to deploy." army) (make-point 5 100))))

(defun do-movement-phase (frame)
  "This function will handle the movement phase."
  (setf (phase-log *application-frame*)
        (concatenate 'string (phase-log *application-frame*)
                     (format *debug-io* "In Movement Phase~%")))
  (setf (frame-current-layout frame) :game-round)
  (setf (phase-log *application-frame*)
        (concatenate 'string (phase-log *application-frame*)
                     "Entering Movement phase.")))

(defun do-combat-phase (frame)
  "This function will handle the combat phase."
  (setf (phase-log *application-frame*)
        (concatenate 'string (phase-log *application-frame*)
                     (format *debug-io* "In Combat Phase~%")))
  (setf (frame-current-layout frame) :game-round)
  (setf (phase-log *application-frame*)
        (concatenate 'string (phase-log *application-frame*)
                     "Entering Combat phase.")))

(defun do-end-phase (frame)
  (setf (phase-log *application-frame*)
        (concatenate 'string (phase-log *application-frame*)
                     (format *debug-io* "In End Phase~%")))
  (incf (turn-number frame))
  (run-end-phase))
