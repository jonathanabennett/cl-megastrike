(in-package :alphastrike)

(deftype elements-type ()
  "This functions like an ENUM, defining the valid list of element types in the game."
  '(member :BM :DS))
(deftype move-type ()
  "This functions like an ENUM, defining the valid ways an element can move in the game."
  '(member :WALK :JUMP))
(deftype crit ()
  "This functions like an ENUM, defining the possible critical hits an element can take in the game."
  '(member :ENGINE :FIRE-CONTROL :MP :WEAPONS))

(defstruct damage-value
  (kind :standard)
  range-brackets :type list)

(defstruct move-value
  (kind :walk)
  distance)

(defmethod display ((obj damage-value))
  "Formats the pilot for display in the record sheet."
  (format nil "~A: ~A" (atk-type obj) (range-brackets obj)))

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
   (move-values
    :documentation "A list of all move values."
    :initarg :mv-list
    :accessor mv-list)
   (role
    :documentation "A text description of the role. This will be used to create formation types
for bonuses."
    :initarg :role
    :accessor role)
   (pilot
    :documentation "A crew object who represents the pilot of the mech. Eventually, we'll need
to handle multiple crew members (or maybe not?), but we'll deal with that then."
    :initarg :pilot
    :accessor pilot
    :type pilot
    :initform (make-instance 'pilot :name "Shooty McShootface" :skill 4))
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
    :documentation "A list of critical hits the unit has sustained. Must be members of the `crit' type."
    :initarg :crits
    :initform '()
    :accessor crits)))

(defun make-element (&key name pv kind size mv-list role attack-list ov current-armor max-armor current-structure max-structure specials crits)
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
                 :mv-list mv-list
                 :role role
                 :attack-list attack-list
                 :ov ov
                 :current-armor current-armor
                 :max-armor max-armor
                 :current-structure current-structure
                 :max-structure max-structure
                 :specials specials
                 :crits crits))
