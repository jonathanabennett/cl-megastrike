(in-package :megastrike)

(defclass pilot ()
  ((name :initarg :name
         :accessor pilot/name)
   (skill :initarg :skill
          :accessor pilot/skill)))

(defun make-pilot (&key name skill)
  (make-instance 'pilot :name name :skill skill))

(defmethod display ((p pilot))
  (format nil "~a (~a)" (pilot/name p) (pilot/skill p)))

(defclass combat-unit ()
  ((cu-mek :initarg :mek
           :accessor cu/mek)
   (cu-full-name :accessor cu/full-name
                 :initarg :full-name)
   (cu-force :initarg :force
             :accessor cu/force)
   (cu-pv-mod :initarg :pv-mod
               :accessor cu/pv-mod)
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

(defun new-combat-unit (&key mek force pv-mod (move-used nil) (cur-armor nil)
                          (cur-struct nil) (crits '()) (target nil) (cur-heat 0)
                          (location nil) pilot)
  (let ((unit-counter 0)
        (full-name "")
        (ca (if cur-armor cur-armor (mek/armor mek)))
        (cs (if cur-struct cur-struct (mek/structure mek))))
    (loop for m being the hash-values of (string-list/source (lobby/units *lobby*))
          if (string= (mek/full-name (cu/mek m)) (mek/full-name mek))
            do (incf unit-counter))
    (setf full-name (if (< 0 unit-counter)
                        (format nil "~a #~a" (mek/full-name mek) unit-counter)
                        (mek/full-name mek)))
    (let* ((cu (make-instance 'combat-unit
                             :full-name full-name
                             :mek mek
                             :force force
                             :pv-mod pv-mod
                             :move-used move-used
                             :cur-armor ca
                             :cur-struct cs
                             :crits crits
                             :target target
                             :cur-heat cur-heat
                             :location location
                             :pilot pilot)))
      (setf (gethash (uuid:make-v5-uuid uuid:+namespace-dns+ (mek/full-name mek)) (game/units *game*)) cu))))

(defun cu/pv (unit)
  (+ (mek/pv (cu/mek unit)) (cu/pv-mod unit)))

(defun cu/size (unit)
  (mek/size (cu/mek unit)))

(defun cu/movement (unit)
  (mek/movement (cu/mek unit)))

(defun print-pilot (cu)
  (display (cu/pilot cu)))

(defun print-force (cu)
  (force/name (cu/force cu)))

(defmethod print-movement ((cu combat-unit))
  (if cu
      (format nil "~{~/megastrike::format-move-assoc/~^/~}" (cu/movement cu))
      "None"))

(defun cu/arm-struct (unit)
  (format nil "~a/~a" (cu/cur-armor unit) (cu/cur-struct unit)))

(defun cu/attack-string (unit)
  (format nil "~a/~a/~a/~a"
          (mek/short-str (cu/mek unit)) (mek/medium-str (cu/mek unit))
          (mek/long-str (cu/mek unit)) (mek/extreme-str (cu/mek unit))))

(defun calculate-pv-modifier (pv skill)
  (let ((skill-diff (- 4 skill)))
    (cond
      ((> 0 skill-diff) (* skill-diff (+ 1 (floor (/ (- pv 5) 10)))))
      ((< 0 skill-diff) (* skill-diff (+ 1 (floor (/ (- pv 3)  5)))))
      (t                0))))

(defun construct-spec-list (specials-str)
  (let ((spec-list '()))
    (dolist (spec (str:words specials-str))
      (push (read-from-string spec) spec-list))
    spec-list))

(defun find-sprite (mek)
  (let ((exact
          (member (mek/full-name (cu/mek mek))
                  (remove-if-not #'(lambda (x) (string= "exact" (car x))) *mechset*)
                  :key #'second
                  :test #'string=))
        (chassis
          (member (mek/chassis (cu/mek mek))
                  (remove-if-not #'(lambda (x) (string= "chassis" (car x))) *mechset*)
                  :key #'second
                  :test #'string=)))
    (format t "~a~%" exact)
    (format t "~a~%" chassis)
    (cond
      (exact (third (car exact)))
      (chassis (third (car chassis)))
      (t     ""))))
