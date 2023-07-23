(in-package :megastrike)

;;; Aspects for Elements
(define-aspect info short-name full-name unit-type role pv size tro (army :initform nil))
(define-aspect damageable cur-armor max-armor cur-struct max-struct crit-list destroyedp)
(define-aspect can-activate
  (selectedp :initform nil)
  (has-acted :initform nil))
(define-aspect moveable move-alist move-used (destination-tile :initform nil))
(define-aspect attacks short medium long (target :initform nil))
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

;;; Element definition, Constructor, and Deconstructor

(define-entity combat-unit (info
                            can-activate
                            damageable
                            moveable
                            attacks
                            heat
                            specials
                            display
                            location
                            pilot))

(defun new-element-from-mul (m &key pname pskill x y)
  (with-slots (short-name long-name unit-type role pv size armor struct mv-string
               short medium long ov display specials-str tro) m
    (let ((mv-cons (construct-mv-alist mv-string))
          (spec-list (construct-spec-list specials-str))
          (u-hex (hex-from-offset :col x :row y)))
      (new-element :short-name short-name :full-name long-name :unit-type unit-type
                   :role role :pv pv :size size :cur-armor armor :max-armor armor
                   :cur-struct struct :max-struct struct :move-list mv-cons
                   :q (hexagon-q u-hex) :r (hexagon-r u-hex) :s (hexagon-s u-hex)
                   :short short :medium medium :long long :ov ov :special-list spec-list
                   :img display :tro tro :pilot pname :skill pskill))))

(defun construct-mv-alist (mv-string)
  (let ((mv-alist '())
         (mv-strings (ppcre:all-matches-as-strings
                      (ppcre:create-scanner "\\d+[a-zA-z]?") mv-string)))
    (dolist (str mv-strings)
      (multiple-value-bind (dist type) (parse-integer str :junk-allowed t)
        (if (= (length str) type)
            (setf mv-alist (acons :walk dist mv-alist))
            (cond
              ((string= "j" (subseq str type))
               (setf mv-alist (acons :jump dist mv-alist)))

              (t
               (setf mv-alist (acons :walk dist mv-alist)))))))
    mv-alist))

(defun construct-spec-list (specials-str)
  (let ((spec-list '()))
    (dolist (spec (str:words specials-str))
      (push (read-from-string spec) spec-list))
    spec-list))

(defun new-element (&key short-name full-name unit-type role pv size
                      cur-armor max-armor cur-struct max-struct
                      move-list short medium long ov (cur-heat 0)
                      special-list (crit-list '()) img tro q r s
                      (pilot "Shooty McGee") (skill 4))
  (let ((arm    (if (eq cur-armor nil) max-armor cur-armor))
        (struct (if (eq cur-struct nil) max-struct cur-struct))
        (asset-path (merge-pathnames img *here*)))
    (create-entity 'combat-unit
                   :info/short-name short-name
                   :info/full-name full-name
                   :info/unit-type unit-type
                   :info/role role
                   :info/pv pv
                   :info/size size
                   :can-activate/selectedp nil
                   :damageable/cur-armor arm
                   :damageable/max-armor max-armor
                   :damageable/cur-struct struct
                   :damageable/max-struct max-struct
                   :damageable/destroyedp nil
                   :moveable/move-alist move-list
                   :moveable/move-used (car (car move-list))
                   :attacks/short short
                   :attacks/medium medium
                   :attacks/long long
                   :heat/ov ov
                   :heat/cur-heat cur-heat
                   :specials/special-list special-list
                   :damageable/crit-list crit-list
                   :display/image-path (make-pattern-from-bitmap-file asset-path)
                   :info/tro tro
                   :location/q q
                   :location/r r
                   :location/s s
                   :pilot/name pilot
                   :pilot/skill skill)))

(defmethod same-entity ((e entity) (o entity))
  (eq (entity-id e) (entity-id o)))

(define-presentation-method present (combat-unit
                                     (type entity)
                                     stream
                                     (view graphical-view) &key)
  (let ((origin (hex-to-pixel (new-hexagon :q (location/q combat-unit)
                                           :r (location/r combat-unit)
                                           :s (location/s combat-unit))
                              (frame/layout *application-frame*)))
        (color (army/color (info/army combat-unit)))
        (layout (frame/layout *application-frame*)))
    (with-translation (stream (* (layout-x-size layout) -0.9)
                              (* (layout-y-size layout) -0.8))
      (draw-pattern* stream (display/image-path combat-unit)
                     (point-x origin) (point-y origin)))
    (with-translation (stream 0 (* (layout-y-size layout) -0.8))
      (surrounding-output-with-border (stream :ink color :filled t :shape :rectangle)
        (if (can-activate/selectedp combat-unit)
            (with-text-style (stream *selected-text-style*)
              (draw-text stream (format nil "~a" (info/short-name combat-unit))
                         origin :align-x :center :align-y :top))
            (draw-text stream (format nil "~a" (info/short-name combat-unit))
                       origin :align-x :center :align-y :top))))))


(define-presentation-method present (combat-unit
                                     (type entity)
                                     stream
                                     (view quickstats-view) &key)
  (quickstats-block stream combat-unit))

(define-presentation-method present (combat-unit
                                     (type entity)
                                     stream
                                     (view textual-view) &key)
  (format stream "~a #~a" (info/full-name combat-unit) (entity-id combat-unit)))
;;; Movement methods

(defun format-move-assoc (stream m colonp atsignp)
  "The colonp and atsignp are required for a function called inside `FORMAT'."
  (declare (ignorable colonp atsignp))
  (format stream "~a~a" (cdr m) (cdr (assoc (car m) *mv-designators*))))

(defmethod format-move ((m moveable))
  (format nil "~{~/megastrike::format-move-assoc/~^/~}" (moveable/move-alist m)))

(defmethod move-lookup ((m moveable) (mv-type symbol))
  (cdr (assoc mv-type (moveable/move-alist m))))

(defmethod move-unit ((unit combat-unit) (destination tile))
  (if (>= (move-lookup unit (moveable/move-used unit))
          (hex-distance (new-hexagon :q (location/q unit)
                                     :r (location/r unit)
                                     :s (location/s unit))
                        (tile-hexagon destination)))
      (progn (set-location unit (tile-hexagon destination))
             (incf (initiative-place *application-frame*))
             (setf (can-activate/has-acted unit) t))))

(defmethod set-location ((unit combat-unit) (loc hexagon))
  (setf (location/q unit) (hexagon-q loc))
  (setf (location/r unit) (hexagon-r loc))
  (setf (location/s unit) (hexagon-s loc)))

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
          (t          6))
        (cond
          ((>= 2 mv)  0)
          ((>= 4 mv)  1)
          ((>= 6 mv)  2)
          ((>= 9 mv)  3)
          ((>= 17 mv) 4)
          (t          5)))))

;;; Attack and Damage

(defmethod calculate-to-hit ((attacker combat-unit) (target combat-unit))
  (let ((range (hex-distance (new-hexagon :q (location/q attacker)
                                          :r (location/r attacker)
                                          :s (location/s attacker))
                             (new-hexagon :q (location/q target)
                                          :r (location/r target)
                                          :s (location/s target)))))
    (cond
      ((>= 3 range) (+ (pilot/skill attacker) (unit-tmm target)))
      ((>= 12 range) (+ (pilot/skill attacker) (unit-tmm target) 2))
      ((>= 21 range) (+ (pilot/skill attacker) (unit-tmm target) 4)))))

(defmethod calculate-damage ((attacker combat-unit) (target combat-unit))
  (let ((range (hex-distance (new-hexagon :q (location/q attacker)
                                          :r (location/r attacker)
                                          :s (location/s attacker))
                             (new-hexagon :q (location/q target)
                                          :r (location/r target)
                                          :s (location/s target)))))
    (cond
      ((>= 3 range) (attacks/short attacker))
      ((>= 12 range) (attacks/medium attacker))
      ((>= 21 range) (attacks/long attacker)))))

(defmethod make-attack ((attacker combat-unit) (target combat-unit))
  (let ((target-num (calculate-to-hit attacker target))
        (to-hit (roll2d)))
    (if (<= target-num to-hit)
        (take-damage target (calculate-damage attacker target))))
  (incf (initiative-place *application-frame*))
  (setf (can-activate/has-acted attacker) t))

(defmethod take-damage ((u damageable) damage)
  (dotimes (x damage)
    (if (eq 0 (damageable/cur-armor u))
        (decf (damageable/cur-struct u))
        (decf (damageable/cur-armor u))))
  (if (>= 0 (damageable/cur-struct u))
      (setf (damageable/destroyedp u) t)))
