(in-package :megastrike)

(defclass force ()
  ((name        :initarg :name   :accessor force/name)
   (color       :initarg :color  :accessor force/color)
   (deploy-zone :initarg :deploy :accessor force/deployment)
   (units       :initarg :units  :accessor force/units)
   (initiative  :initform nil    :accessor force/initiative)))

(defun new-force (name color deploy &optional (unit-list '()))
  (let ((a (make-instance 'force
                       :name name
                       :color color
                       :deploy deploy
                       :units unit-list)))
    a))

(defmethod same-force ((a force) (o force))
  (string= (force/name a) (force/name o)))

(defmethod same-force ((a force) (s string))
  (string= (force/name a) s))

(defmethod same-force ((s string) (a force))
  (string= (force/name a) s))

(defmethod same-force ((a force) o)
  nil)

(defmethod same-force (o (a force))
  nil)

(defmethod add-force ((g game) (f force))
  (push f (game/forces g)))

(defmethod add-unit ((a force) (u combat-unit))
  (setf (info/force u) a))

(defmethod count-units ((a force))
  (let ((count 0))
    (map-entities #'(lambda (e) (if (same-force (info/force e) a) (incf count))))
    count))

(defmethod force-pv ((f force))
  (let ((total 0))
    (map-entities #'(lambda (e)
                      (if (same-force (info/force e) f) (incf total (info/pv e)))))
    total))

(defmethod turn-order-list ((a force))
  (let ((force-string (force/name a))
        (order '()))
  (dotimes (i (count-units a))
    (push force-string order))
    order))


;; (define-presentation-method present (force
;;                                    (type force)
;;                                    stream
;;                                    (view textual-view) &key)
;;   (if (same-force force (game/selected-force (lobby/game *lobby*)))
;;       (with-text-style (stream *selected-text-style*)
;;         (format stream "~a" (force/name force)))
;;       (format stream "~a" (force/name force))))
