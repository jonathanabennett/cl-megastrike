(in-package :megastrike)

(defclass force ()
  ((name        :initarg :name   :accessor force/name)
   (color       :initarg :color  :accessor force/color)
   (deploy-zone :initarg :deploy :accessor force/deployment)
   (units       :initarg :units  :accessor force/units)
   (initiative  :initform nil    :accessor force/initiative)))

(defun new-force (name color deploy &optional (unit-list '()))
  (let ((f (make-instance 'force
                       :name name
                       :color color
                       :deploy deploy
                       :units unit-list)))
    f))

(defmethod same-force ((f force) (o force))
  (string= (force/name f) (force/name o)))

(defmethod same-force ((f force) (s string))
  (string= (force/name f) s))

(defmethod same-force ((s string) (f force))
  (string= (force/name f) s))

(defmethod same-force ((f force) o)
  nil)

(defmethod same-force (o (f force))
  nil)

(defmethod count-units ((f force))
  (let ((count 0))
    (maphash #'(lambda (k v) (if (same-force (cu/force v) f) (incf count))) (game/units *game*))
    count))

(defmethod force-pv ((f force))
  (let ((total 0))
    (maphash #'(lambda (k v)
                 (if (same-force (cu/force v) f) (incf total (cu/pv v)))) (game/units *game*))
    total))

(defmethod turn-order-list ((f force))
  (let ((force-string (force/name f))
        (order '()))
  (dotimes (i (count-units f))
    (push force-string order))
    order))
