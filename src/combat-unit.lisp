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
   (cu-destination :initform nil
                   :accessor cu/destination)
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
   (cu-actedp :initform nil
              :accessor cu/actedp)
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
    (setf (cu/display cu) (namestring (truename image-file)))))

(defmethod move-lookup (m (mv-type symbol))
  nil)

(defmethod move-lookup ((m combat-unit) (mv-type symbol))
  (if mv-type
      (cdr (assoc mv-type (cu/movement m)))
      (cdr (first (cu/movement m)))))

;; TODO This method currently only uses manhattan distance.
;; I need to rewrite this with a pathfinding routine.
;; That will likely require adjustments to the `Tile' class.

(defmethod move-unit ((cu combat-unit) (destination hexagon))
  (when (>= (move-lookup cu (cu/move-used cu))
            (hex-distance (cu/location cu) destination))
    (setf (cu/destination cu) destination)))

(defmethod calculate-to-hit ((attacker combat-unit) (target combat-unit))
  (let ((skill (pilot/skill (cu/pilot attacker)))
        (attacker (calculate-attacker-mod attacker target))
        (target (calculate-target-mod attacker target))
        (other (calculate-other-mod attacker target))
        (range (calculate-range-mod attacker target)))
    (+ skill attacker target other range)))

(defmethod calculate-attacker-mod ((attacker combat-unit) (target combat-unit))
  (let ((mod 0)
        (move (cu/move-used attacker)))
    (when (or (eql move :stand-still) (eql move :immobile))
      (decf mod))
    (when (eql move :jump)
      (incf mod 2))
    mod))

(defmethod calculate-target-mod ((attacker combat-unit) (target combat-unit))
  (let ((tmm (mek/tmm (cu/mek target)))
        (move (cu/move-used target)))
    (when (eql move :jump)
      (incf tmm))
    (when (eql move :stand-still)
      (setf tmm 0))
    (when (eql move :immobile)
      (setf tmm -4))
    tmm))

(defmethod calculate-other-mod ((attacker combat-unit) (target combat-unit))
  (let ((partial 0)
        (woods 0)
        (heat (cu/cur-heat attacker))
        (criticals 0)
        (abilities 0))
    (+ partial woods heat criticals abilities)))

(defmethod calculate-range-mod ((attacker combat-unit) (target combat-unit))
  (let ((range (hex-distance (cu/location attacker) (cu/location target))))
    (cond
      ((>= 3 range) 0)
      ((>= 12 range) 2)
      ((>= 21 range) 4)
      ((>= 30 range) 6))))

(defmethod print-damage ((attacker combat-unit) range)
  (cond
    ((>= 3 range) (mek/short-str (cu/mek attacker)))
    ((>= 12 range) (mek/medium-str (cu/mek attacker)))
    ((>= 21 range) (mek/long-str (cu/mek attacker)))
    ((>= 42 range) (mek/extreme-str (cu/mek attacker)))))

(defmethod calculate-damage ((attacker combat-unit) (target combat-unit))
  (let ((range (hex-distance (cu/location attacker) (cu/location target)))
        (short (mek/short (cu/mek attacker)))
        (med (mek/medium (cu/mek attacker)))
        (long (mek/long (cu/mek attacker)))
        (extreme (mek/extreme (cu/mek attacker))))
    (when (and (mek/short* (cu/mek attacker)) (<= 3 (roll 1)))
      (setf short 1))
    (when (and (mek/medium* (cu/mek attacker)) (<= 3 (roll 1)))
      (setf med 1))
    (when (and (mek/long* (cu/mek attacker)) (<= 3 (roll 1)))
      (setf long 1))
    (when (and (mek/extreme* (cu/mek attacker)) (<= 3 (roll 1)))
      (setf extreme 1))
    (cond
      ((>= 3 range) short)
      ((>= 12 range) med)
      ((>= 21 range) long)
      ((>= 30 range) extreme))))

(defmethod take-damage ((cu combat-unit) damage)
  (dotimes (x damage)
    (if (eq 0 (cu/cur-armor cu))
        (decf (cu/cur-struct cu))
        (decf (cu/cur-armor cu))))
  (when (>= 0 (cu/cur-struct cu))
    (setf (cu/destroyedp cu) t)))

(defmethod make-attack ((attacker combat-unit) (target combat-unit))
  (let ((target-num (calculate-to-hit attacker target))
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
