(in-package :alphastrike)

(defun roll-initiative ()
  (let ((red-init  (roll2d))
        (blue-init (roll2d)))
    (format nil "Red: ~8a blue: ~8a" red-init blue-init)))
    ;; (cond
    ;;   ((> red-init blue-init) '(red blue))
    ;;   ((> blue-init red-init) '(blue red))
    ;;   (t                       (roll-initiative)))))

(defun do-end-phase (frame)
  (setf (current-phase frame) 0)
  (incf (turn-number frame))
  (run-end-phase))

(defun do-phase (frame)
  (incf (current-phase *application-frame*))
  (notify-user frame (format nil "Phase number: ~a" (current-phase frame))))
