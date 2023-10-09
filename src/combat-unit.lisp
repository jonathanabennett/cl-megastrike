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
   (cu-display :initarg :display
               :accessor cu/display)
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
   (cu-destroyedp :initform nil
                  :accessor cu/destroyedp)
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
    (dolist (m (game/units *game*))
      (when (string= (mek/full-name (cu/mek m)) (mek/full-name mek))
        (incf unit-counter)))

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
      (find-sprite cu)
      cu)))

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

(defun find-sprite (cu)
  (let ((exact
          (member (mek/full-name (cu/mek cu))
                  (remove-if-not #'(lambda (x) (string= "exact" (car x))) *mechset*)
                  :key #'second
                  :test #'string=))
        (chassis
          (member (mek/chassis (cu/mek cu))
                  (remove-if-not #'(lambda (x) (string= "chassis" (car x))) *mechset*)
                  :key #'second
                  :test #'string=))
        (image-file "")
        (image-folder (merge-pathnames "images/units/" *data-folder*)))
    (cond
      (exact (setf image-file (merge-pathnames (third (car exact)) image-folder)))
      (chassis (setf image-file (merge-pathnames (third (car chassis)) image-folder)))
      (t     (setf image-file (merge-pathnames "defaults/default_medium.png" image-folder))))
    (format t "filename: ~a" image-file)
    (setf (cu/display cu) (gdk:make-texture :path (namestring (truename image-file))))))

(defmethod move-unit ((cu combat-unit) (destination tile))
  (when (>= (move-lookup cu (cu/move-used cu))
            (hex-distance (cu/location cu) destination))
    (setf (cu/location cu) destination)
    (incf (game/initiative-place *game*))
    (setf (game/phase-log *game*)
          (concatenate 'string (game/phase-log *game*)
                       (format nil "~a has moved to ~a.~%"
                               (info/full-name unit) (offset-from-hex destination))))))

(defmethod unit-tmm ((cu combat-unit))
  (unless (cu/move-used cu)
    (setf (cu/move-used cu) (car (car (cu/movement cu)))))
  (if (eq (cu/move-used cu) :jump)
      (1+ (mek/tmm (cu/mek cu)))
      (mek/tmm (cu/mek cu))))

(defmethod calculate-to-hit ((attacker combat-unit) (target combat-unit))
  (let ((range (hex-distance (cu/location attacker) (cu/location target))))
    (cond
      ((>= 3 range) (+ (pilot/skill attacker) (unit-tmm target)))
      ((>= 12 range) (+ (pilot/skill attacker) (unit-tmm target) 2))
      ((>= 21 range) (+ (pilot/skill attacker) (unit-tmm target) 4))
      ((>= 30 range) (+ (pilot/skill attacker) (unit-tmm target) 6)))))

(defmethod calculate-damage ((attacker combat-unit) (target combat-unit))
  (let ((range (hex-distance (cu/location attacker) (cu/location target)))
        (short (mek/short (cu/mek attacker)))
        (med (mek/medium (cu/mek attacker)))
        (long (mek/long (cu/mek attacker)))
        (extreme (mek/extreme (cu/mek attacker))))
    (when (and (mek/short* (cu/mek attacker)) (<= 3 (roll 1)))
      (setf short 1))
    (when (and (mek/medium* (cu/mek attacker)) (<= 3 (roll 1)))
      (setf medium 1))
    (when (and (mek/long* (cu/mek attacker)) (<= 3 (roll 1)))
      (setf long 1))
    (when (and (mek/extreme* (cu/mek attacker)) (<= 3 (roll 1)))
      (setf extreme 1))
    (cond
      ((>= 3 range) short)
      ((>= 12 range) medium)
      ((>= 21 range) long)
      ((>= 30 range) extreme))))

(defmethod take-damage ((cu combat-unit) damage)
  (dotimes (x damage)
    (if (eq 0 (cu/cur-armor cu))
        (decf (cu/cur-struct cu))
        (incf (cu/cur-armor cu))))
  (when (>= 0 (cu/cur-struct cu))
    (setf (cu/destroyedp cu) t))
  (setf (game/phase-log *game*)
        (concatenate 'string (game/phase-log *game*)
                     (format nil "~a now has ~a armor and ~a structure.~%"
                             (info/full-name u)
                             (damageable/cur-armor u)
                             (damageable/cur-struct u)))))

(defmethod make-attack ((attacker combat-unit) (target combat-unit))
  (let ((target-num (calcuate-to-hit attacker target))
        (to-hit (roll2d))
        (log-string ""))
    (setf log-string (concatenate `string log-string
                                  (format nil "~a attacking ~a (needs a ~a): Rolled ~a~%"
                                          (cu/full-name attacker)
                                          (cu/full-name target)
                                          target-num
                                          to-hit)))
    (when (<= target-num to-hit)
      (take-damage target (calculate-damage attacker target)))
    (incf (game/initiative-place *game*))))
