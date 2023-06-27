(in-package :alphastrike)

(deftype elements-type ()
  "This functions like an ENUM, defining the valid list of element types in the game."
  '(member 'BM 'DS))
(deftype move-type ()
  "This functions like an ENUM, defining the valid ways an element can move in the game."
  '(member 'WALK 'JUMP))
(deftype crit ()
  "This functions like an ENUM, defining the possible critical hits an element can take in the game."
  '(member 'ENGINE 'FIRE-CONTROL 'MP 'WEAPONS))

(defclass damage-value ()
  ((kind
    :documentation "What kind of attack is this?"
    :initarg :kind
    :accessor kind)
   (attack
    :documentation "A list of numbers representing the damage done at short/medium/long/extreme."
    :initarg :attack
    :accessor attack)))

(defun make-attack (&key kind attack)
  (make-instance 'damage-value
                 :kind kind
                 :attack attack))

;; TODO Remove and replace this.
(defstruct move-value
  (kind :walk)
  distance)

(defmethod display ((obj damage-value))
  "Formats the pilot for display in the record sheet."
  (format nil "~A: ~A" (kind obj) (attack obj)))

(defclass combat-unit ()
  ((id
    :documentation "UUID identifying the unit"
    :initarg :id
    :accessor id)
   (element
    :documentation "The Element"
    :initarg :element
    :accessor element)
   (mv-status
    :documentation "The unit's movement status."
    :initform nil
    :accessor mv-status)
   (attack-status
    :documentation "The unit's attack status."
    :initform nil
    :accessor atk-status)
   (pilot
    :documentation "The pilot of the unit."
    :initarg :pilot
    :accessor pilot
    :initform (make-instance 'pilot :name "Shooty McShootface" :skill 4))
   ))

(defclass element ()
  ((name
    :documentation "The name of the element. For exampe: Locust lct-1v"
    :initarg :name
    :accessor name)
   (point-value
    :documentation "The point value of the element."
    :initarg :pv
    :accessor pv)
   (kind
    :documentation "The unit type, must be one of the options from the element-type enum."
    :initarg :kind
    :type element-type
    :accessor kind)
   (size
    :documentation "The size. Should be an integer from 1-4."
    :initarg :size
    :accessor size)
   ;; TODO Redesign class so that it has primary and secondary move slots
   (primary-move
    :documentation "The kind of move that the unit can make."
    :initarg :prime-move
    :accessor prime-move)
   (primary-distance
    :documentation "The distance of the primary move."
    :initarg :prime-distance
    :accessor prime-distance)
   (secondary-move
    :documentation "The kind of secondary movement that the unit can make."
    :initarg :secondary-move
    :initform nil
    :accessor secondary-move)
   (secondary-distance
    :documentation "The distance of the secondary move."
    :initarg :secondary-distance
    :initform nil
    :accessor secondary-distance)
   (role
    :documentation "A text description of the role. This will be used to create formation types
for bonuses."
    :initarg :role
    :accessor role)
   (attack-list
    :documentation "A list of damage-value objects. Must include one with a `:standard'."
    :initarg :attack-list
    :accessor attack-list)
   (overheat
    :documentation "How much extra damage the unit can generate in exchange for gaining heat."
    :initarg :ov
    :accessor overheat)
   (heat
    :documentation "How much heat the unit currently has."
    :initarg :heat
    :initform 0
    :accessor heat)
   (max-armor
    :documentation "Maximum armor value."
    :initarg :max-armor
    :accessor armor)
   (current-armor
    :documentation "Current armor value."
    :initarg :current-armor
    :accessor current-armor)
   (max-structure
    :documentation "Maximum structure value."
    :initarg :max-structure
    :accessor struct)
   (current-structure
    :documentation "Current structure value."
    :initarg :current-structure
    :accessor current-struct)
   (specials
    :documentation "A list of special abilities the unit has."
    :initarg :specials
    :accessor specials)
   (crits
    ;; TODO Reconsider whether a simple list is the best way to store these.
    :documentation "A list of critical hits the unit has sustained. Must be members of the `crit' type."
    :initarg :crits
    :initform '()
    :accessor crits)))

(defun make-element (&key name pv kind size role
                       prime-move prime-distance secondary-move secondary-distance
                       attack-list ov
                       current-armor max-armor
                       current-structure max-structure
                       specials crits)
  "Make an element, checking for values typically missing such as current-armor and setting them to reasonable defaults."
  (if (eq current-armor nil)
       (setf current-armor max-armor))
   (if (eq current-structure nil)
       (setf current-structure max-structure))
   (make-instance 'element
                 :name name
                 :pv pv
                 :kind kind
                 :size size
                 :prime-move prime-move
                 :prime-distance prime-distance
                 :secondary-move secondary-move
                 :secondary-distance secondary-distance
                 :role role
                 :attack-list attack-list
                 :ov ov
                 :current-armor current-armor
                 :max-armor max-armor
                 :current-structure current-structure
                 :max-structure max-structure
                 :specials specials
                 :crits crits))

(defun make-combat-unit (&key id pilot unit)
  (if (eq id nil)
      (setf id (random-uuid::make-uuid)))
  (if (eq pilot nil)
      (setf pilot (make-instance 'pilot :skill 4 :name "Shooty McPilotFace")))
  (make-instance 'combat-unit
                :id id
                :pilot pilot
                :element unit))

(defmethod unit-tmm ((unit combat-unit))
  "This will return the TMM of a unit based on it's current mv-status."
  1)

;; (cond
;;   ((>= 2 *test-num*) (format t "branch 1"))
;;   ((>= 4 *test-num*) (format t "branch 2"))
;;   ((>= 6 *test-num*) (format t "branch 3"))
;;   ((>= 8 *test-num*) (format t "branch 4")))

;; Can you figure out how this format control string works to replace the below with
;; "~{~@[~a~^/~]~^~}"

(defun format-move (element)
  (let ((mv-string ""))
    (when (eq (prime-move element) :walk)
        (setf mv-string (concatenate 'string mv-string (format nil "~a" (prime-distance element)))))
    (when (eq (secondary-move element) :jump)
        (setf mv-string (concatenate 'string mv-string (format nil "/~aJ" (secondary-distance element)))))
    mv-string))

(defun element->json (element)
  (with-output-to-string (str)
    (cl-json:with-object (str)
      (cl-json:encode-object-member "name" (name element) str)
      (cl-json:encode-object-member "pv"   (pv element) str)
      (cl-json:encode-object-member "kind" (kind element) str)
      (cl-json:encode-object-member "size" (size element) str)
      (cl-json:encode-object-member "prime-move" (prime-move element) str)
      (cl-json:encode-object-member "prime-distance" (prime-distance element) str)
      (cl-json:encode-object-member "secondary-move" (secondary-move element) str)
      (cl-json:encode-object-member "secondary-distance" (secondary-distance element) str)
      (cl-json:encode-object-member "role" (role element) str)
      (cl-json:encode-object-member "attack-list" (attack-list element) str)
      (cl-json:encode-object-member "ov" (overheat element) str)
      (cl-json:encode-object-member "current-armor" (current-armor element) str)
      (cl-json:encode-object-member "max-armor" (armor element) str)
      (cl-json:encode-object-member "current-structure" (current-struct element) str)
      (cl-json:encode-object-member "max-structure" (struct element) str)
      (cl-json:encode-object-member "specials" (specials element) str)
      (cl-json:encode-object-member "crits" (crits element) str))
    str))

(defun json->element (input)
  "Turns a string of valid `json-data' into an element ."
  (let ((json-data (cl-json:decode-json-from-string input)))
    (make-element :name (cdr (assoc :name json-data))
                  :pv (cdr (assoc :pv json-data))
                  :kind (cdr (assoc :kind json-data))
                  :size (cdr (assoc :size json-data))
                  :prime-move (cdr (assoc :prime-move json-data))
                  :prime-distance (cdr (assoc :prime-distance json-data))
                  :secondary-move (cdr (assoc :secondary-move json-data))
                  :secondary-distance (cdr (assoc :secondary-distance json-data))
                  :role (cdr (assoc :role json-data))
                  :attack-list (cdr (assoc :attack-list json-data))
                  :ov (cdr (assoc :ov json-data))
                  :current-armor (cdr (assoc :current-armor json-data))
                  :max-armor (cdr (assoc :max-armor json-data))
                  :current-structure (cdr (assoc :current-struct json-data))
                  :max-structure (cdr (assoc :max-structure json-data))
                  :specials (cdr (assoc :specials json-data))
                  :crits (cdr (assoc :crits json-data)))))
