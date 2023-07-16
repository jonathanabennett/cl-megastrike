(in-package :megastrike)

(defvar *armies* '())

(defclass army ()
  ((name
    :initarg :name
    :accessor army/name)
   (color
    :initarg :color
    :accessor army/color)
   (units
    :initarg :units
    :accessor army/units)
   (initiative
    :initform nil
    :accessor army/initiative)))

(defun new-army (name color &optional (unit-list '()))
  (push (make-instance 'army
                       :name name
                       :color color
                       :units unit-list)
        *armies*))

(defmethod add-unit ((a army) (u combat-unit))
  (push u (army/units a)))

(defmethod count-units ((a army))
  (length (army/units a)))

(defmethod draw-units (stream (a army))
  (with-drawing-options (stream :ink (army/color a))
    (dolist (u (army/units a))
      (present u 'combat-unit :stream stream))))

(defmethod turn-order-list ((a army))
  (let ((army-string (army/name a))
        (order '()))
  (dotimes (i (count-units a))
    (push army-string order))
    order))
