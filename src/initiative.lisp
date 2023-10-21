(in-package :megastrike)

(defun initiative-phase-click (hex hex-units)
  nil)

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
