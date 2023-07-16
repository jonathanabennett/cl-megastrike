(in-package :alphastrike)

(defun do-end-phase (frame)
  (setf (current-phase frame) 0)
  (incf (turn-number frame))
  (run-end-phase))

(defun do-phase (frame)
  (incf (current-phase *application-frame*)))

(defun roll-initiative (army-list)
  (mapcar #'(lambda (a) (setf (army/initiative a) (roll2d))) army-list)
  (format *debug-io* "~a rolled ~a, ~a rolled ~a" (army/name (first army-list))
                                                  (army/initiative (first army-list))
                                                  (army/name (second army-list))
                                                  (army/initiative (second army-list)))
  (if (eql 2 (length (remove-duplicates army-list :test #'(lambda (a b)
                                                           (eql (army/initiative a)
                                                                (army/initiative b))))))
      (build-initiative-order army-list)
      (roll-initiative army-list))
  )

(defun build-initiative-order (army-list)
  (let* ((army-order (sort army-list #'(lambda (a b) (< (army/initiative a)
                                                       (army/initiative b))))))

    (make-turn-order (turn-order-list (first army-order))
                     (turn-order-list (second army-order)))))

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
