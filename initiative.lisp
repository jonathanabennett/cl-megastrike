(in-package :alphastrike)

(defun roll-initiative ()
  (let ((red-init  (roll2d))
        (blue-init (roll2d)))
    (format nil "Red: ~8a blue: ~8a" red-init blue-init)))
    ;; (cond
    ;;   ((> red-init blue-init) '(red blue))
    ;;   ((> blue-init red-init) '(blue red))
    ;;   (t                       (roll-initiative)))))
