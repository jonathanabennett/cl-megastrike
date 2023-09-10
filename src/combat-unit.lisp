(in-package :megastrike)

(defclass pilot ()
  ((name :initarg :name
         :accessor pilot/name)
   (skill :initarg :skill
          :accessor pilot/skill)))

(defun make-pilot (&key name skill)
  (make-instance 'pilot :name name :skill skill))

(defclass combat-unit ()
  ((cu-mek :initarg :mek
           :accessor cu/mek)
   (cu-force :initarg :force
             :accessor cu/force)
   (cu-pv-mult :initarg :pv-mult
               :accessor cu/pv-mult)
   (cu-move-used :initarg :move-used
                 :accessor cu/move-used)
   (cu-cur-armor :initarg :cur-armor
                 :accessor cu/cur-armor)
   (cu-cur-struct :initarg :cur-struct
                  :accessor cu/cur-struct)
   (cu-crits :initarg :crits
             :accessor cu/crits)
   (cu-target :initarg :target
              :accessor cu/target)
   (cu-cur-heat :initarg :cur-heat
                :accessor cu/cur-heat)
   (cu-location :initarg :location
                :accessor cu/location)
   (cu-pilot :initarg :pilot
             :accessor cu/pilot)))

(defun make-combat-unit (&key mek force pv-mult (move-used nil) (cur-armor nil)
                           (cur-struct nil) (crits '()) (target nil) (cur-heat 0)
                           (location nil) pilot)
  (let ((ca (if cur-armor cur-armor (mek/armor mek)))
        (cs (if cur-struct cur-struct (mek/structure mek))))
    (make-instance 'mek
                   :mek mek
                   :force force
                   :pv-mult pv-mult
                   :move-used move-used
                   :cur-armor ca
                   :cur-struct cs
                   :crits crits
                   :target target
                   :cur-heat cur-heat
                   :location location
                   :pilot pilot)))
