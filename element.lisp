(in-package :alphastrike)

(deftype elements-type ()
  "This defines the valid list of element types in the game."
  '(member 'BM 'DS))
(deftype move-type ()
  "This defines the valid ways an element can move in the game."
  '(member 'WALK 'JUMP))
(defvar *mv-designators* '((:walk . "")
                           (:jump . "j")))
(deftype crit ()
  "This defines the possible critical hits an element can take in the game."
  '(member 'ENGINE 'FIRE-CONTROL 'MP 'WEAPONS))

(define-aspect info short-name full-name unit-type role pv size tro)
(define-aspect damageable cur-armor max-armor cur-struct max-struct crit-list)
(define-aspect moveable move-alist move-used)
(define-aspect attacks short medium long)
(define-aspect heat ov cur-heat)
(define-aspect specials special-list)
(define-aspect display
  (image-path :initform nil))
(define-aspect location
  (q :initform nil)
  (r :initform nil)
  (s :initform nil))
(define-aspect pilot
    (name :initform nil) (skill :initform nil))

(define-entity combat-unit (info damageable moveable attacks heat specials display location pilot))

(defun new-element (&key short-name full-name unit-type role pv size
                      cur-armor max-armor cur-struct max-struct
                      move-list short medium long ov (cur-heat 0)
                      special-list (crit-list '()) img tro q r s
                      (pilot "Shooty McGee") (skill 4))
  (let ((arm    (if (eq cur-armor nil) max-armor cur-armor))
        (struct (if (eq cur-struct nil) max-armor cur-armor)))
    (create-entity 'combat-unit
                   :info/short-name short-name
                   :info/full-name full-name
                   :info/unit-type unit-type
                   :info/role role
                   :info/pv pv
                   :info/size size
                   :damageable/cur-armor arm
                   :damageable/max-armor max-armor
                   :damageable/cur-struct struct
                   :damageable/max-struct max-struct
                   :moveable/move-alist move-list
                   :moveable/move-used (car (car move-list))
                   :attacks/short short
                   :attacks/medium medium
                   :attacks/long long
                   :heat/ov ov
                   :heat/cur-heat cur-heat
                   :specials/special-list special-list
                   :damageable/crit-list crit-list
                   :display/image-path img
                   :info/tro tro
                   :location/q q
                   :location/r r
                   :location/s s
                   :pilot/name pilot
                   :pilot/skill skill)))

(defun locust-lct-1v ()
  (new-element
   :short-name "LCT-1V"
   :full-name "Locust LCT-1V"
   :unit-type :BM
   :role :scout
   :pv 18
   :size 1
   :max-armor 2
   :max-struct 2
   :move-list (list
               (cons :walk 8)
              )
   :short 1
   :medium 1
   :long 0 ;; Enter 0.5 for 0*
   :ov 0
   :cur-heat 0
   :special-list '(:SRCH :SOA)
   :crit-list '()
   :img #P"data/images/units/defaults/default_light.png"
   :tro
"Overview: The Locust is undoubtedly one of the most popular and prevalent
light BattleMechs ever made. First produced in 2499, the almost dozen distinct
factories manufacturing the design quickly spread the design to every power in
human space. Its combination of tough armor (for its size), exceptional speed,
and most importantly, low cost have all contributed to the Locust's success. It
remains the benchmark for many scouting designs, and its continual upgrades have
ensured that it remains just as effective with every new conflict that appears.

Capabilities: As the Locust was first developed as a recon platform, speed is
paramount to the design's philosophy. While many variants change the weaponry to
fill specific tasks or purposes, Locusts are nearly always pressed into service
in ways where they can best take advantage of their speed. When in line
regiments, they can act as a deadly flankers or harassers, and are often used in
reactionary roles to quickly plug holes in a fluid battle line. The structural
form of Locusts themselves are their greatest weakness; with no hands, they are
disadvantaged in phyisical combat and occasionally have difficulty righting
themselves after a fall.

Deployment: One of the most common designs even produced, even the smallest
mercenary or pirate outfits will often field one or more of the design.
Production for the Locust has continued uninterrupted for centuries, and it
plays an important role in the militaries of many smaller nations. The base
LCT-1V was once estimated to account for more than 75% of all Locusts in
existence at the end of the Succession Wars, though these numbers have dropped
with the reappearance of more advanced technology. Still, it remains common in
every military worth note.

systemmanufacturer:CHASSIS:Bergan
systemmode:CHASSIS:VII
systemmanufacturer:ENGINE:LTV
systemmode:ENGINE:160
systemmanufacturer:ARMOR:StarSlab
systemmode:ARMOR:/1
systemmanufacturer:COMMUNICATIONS:Garrett
systemmode:COMMUNICATIONS:T10-B
systemmanufacturer:TARGETING:O/P
systemmode:TARGETING:911 "
))


(define-system log-all-entities ((entity))
  (print (location/q entity)))

(defun format-move-assoc (stream m colonp atsignp)
  (format stream "~a~a" (cdr m) (cdr (assoc (car m) *mv-designators*))))

(defmethod format-move ((m moveable))
  (format nil "~{~/alphastrike::format-move-assoc/~^/~}" (moveable/move-alist m)))

(defmethod unit-tmm ((m moveable))
  (if (not (moveable/move-used m))
      (setf (moveable/move-used m) (car (car (moveable/move-alist m)))))
  (let ((mv (cdr (assoc (moveable/move-used m) (moveable/move-alist m))))
        (mv-type (car (assoc (moveable/move-used m) (moveable/move-alist m)))))
    (if (eq mv-type :jump)
        (cond
          ((>= 2 mv)  1)
          ((>= 4 mv)  2)
          ((>= 6 mv)  3)
          ((>= 9 mv)  4)
          ((>= 17 mv) 5)
          ((t)        6))
        (cond
          ((>= 2 mv)  0)
          ((>= 4 mv)  1)
          ((>= 6 mv)  2)
          ((>= 9 mv)  3)
          ((>= 17 mv) 4)
          ((t)        5)))))

(defmethod take-damage ((u damageable))
  (if (eq 0 (damageable/cur-armor u))
      (decf (damageable/cur-struct u))
      (decf (damageable/cur-armor u))))
