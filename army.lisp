(in-package :alphastrike)

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

(defun new-army (name color &optional (unit-list nil))
  (push (make-instance 'army
                       :name name
                       :color color
                       :units unit-list)
        *armies*))

(defmethod add-unit ((a army) (u combat-unit))
  (push u (army/units a)))

(defmethod count-units ((a army))
  (length (army/unit a)))

(defmethod draw-units (stream (a army))
  (dolist (u (army/units a))
    (present u 'combat-unit :stream stream)))

REPLACE ALL REFERENCES TO TEAM/TEAMS WITH ARMY/ARMIES
