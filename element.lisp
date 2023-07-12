(in-package :alphastrike)

(deftype elements-type ()
  "This defines the valid list of element types in the game."
  '(member BM DS))
(deftype move-type ()
  "This defines the valid ways an element can move in the game."
  '(member walk jump))
(defvar *mv-designators* '(('walk . "")
                           ('jump . "j")))
(deftype crit ()
  "This defines the possible critical hits an element can take in the game."
  '(member 'ENGINE 'FIRE-CONTROL 'MP 'WEAPONS))

;;; Aspects for Elements

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

;;; Element definition and Constructor

(define-entity combat-unit (info
                            damageable
                            moveable
                            attacks
                            heat
                            specials
                            display
                            location
                            pilot))

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

;;; Systems operating on Elements

(define-system log-all-entities ((entity))
  (print (location/q entity)))

(define-system draw-units ((entity display location))
  (draw-text (find-pane-named *application-frame* 'world)
             (format nil "~a" (info/short-name entity))
             (hex-to-pixel (new-hexagon :q (location/q entity)
                                        :r (location/r entity)
                                        :s (location/s entity)) *layout*)
             :align-x :center))

