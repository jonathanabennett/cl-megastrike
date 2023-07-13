(in-package :alphastrike)

(defun roll (dice &optional (mods 0))
  (let ((ret mods))
    (dotimes (x dice) (incf ret (+ (random 6) 1)))
    ret))

(defun roll2d (&optional (mods 0))
  (roll 2 0))
