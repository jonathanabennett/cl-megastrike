(in-package :megastrike)

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
  (let ((a (make-instance 'army
                       :name name
                       :color color
                       :units unit-list)))
    (push a (frame/armies *application-frame*))
    a))

(defmethod same-army ((a army) (o army))
  (string= (army/name a) (army/name o)))

(defmethod same-army ((a army) (s string))
  (string= (army/name a) s))

(defmethod add-unit ((a army) (u combat-unit))
  (setf (info/army u) a))

(defmethod count-units ((a army))
  (let ((count 0))
    (map-entities #'(lambda (e) (if (same-army (info/army e) a) (incf count))))
    count))

(defmethod turn-order-list ((a army))
  (let ((army-string (army/name a))
        (order '()))
  (dotimes (i (count-units a))
    (push army-string order))
    order))

(define-presentation-method present (army
                                   (type army)
                                   stream
                                   (view textual-view) &key)
  (format stream "~a" (army/name army) (army/color army)))
