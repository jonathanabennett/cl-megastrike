(in-package :megastrike)

;;; Aspects for Elements
(define-aspect info chassis model type role pv size (force :initform nil))
(define-aspect can-activate
  (selectedp :initform nil)
  (has-acted :initform nil))
(define-aspect moveable move-alist tmm move-used)
(define-aspect damageable cur-armor max-armor cur-struct max-struct crit-list destroyedp)
(define-aspect attacks short medium long extreme (target :initform nil))
(define-aspect heat ov cur-heat)
(define-aspect specials abilities)

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

(defun find-sprite (mek)
  (let ((first-pass (fuzzy-match:fuzzy-match (mek/chassis mek) (uiop:directory-files (merge-pathnames "data/images/units/mechs" *here*)))))
    (if (search (mek/model mek) (namestring (first (fuzzy-match:fuzzy-match (mek/model mek) first-pass))))
        (first (fuzzy-match:fuzzy-match (mek/model mek) first-pass))
        (first first-pass))))

(defun new-element-from-mul (m &key pname pskill)
  (with-slots (chassis model role unit-type size movement tmm armor structure
               ov pv abilities) m
    (let ((spec-list (construct-spec-list abilities))
          (display (find-sprite m)))
      (new-element :chassis chassis :model model :type unit-type :role role :pv pv
                   :tmm tmm :size size :cur-armor armor :max-armor armor
                   :cur-struct structure :max-struct structure :move-list movement
                   :short (mek/comparable-short m) :medium (mek/comparable-medium m)
                   :long (mek/comparable-long m) :extreme (mek/comparable-extreme m)
                   :ov ov :special-list spec-list :img (find-sprite m) :pilot pname
                   :skill pskill))))

(defun construct-spec-list (specials-str)
  (let ((spec-list '()))
    (dolist (spec (str:words specials-str))
      (push (read-from-string spec) spec-list))
    spec-list))

(defun new-element (&key chassis model type role pv size cur-armor max-armor tmm
                      cur-struct max-struct move-list short medium long extreme
                      ov (cur-heat 0) special-list (crit-list '()) img q r s
                      (pilot "Shooty McGee") (skill 4))
  (let ((arm    (if (eq cur-armor nil) max-armor cur-armor))
        (struct (if (eq cur-struct nil) max-struct cur-struct))
        (asset-path (merge-pathnames img *here*)))
    (create-entity 'combat-unit
                   :info/chassis chassis
                   :info/model model
                   :info/type type
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
                   :moveable/tmm tmm
                   :attacks/short short
                   :attacks/medium medium
                   :attacks/long long
                   :attacks/extreme extreme
                   :heat/ov ov
                   :heat/cur-heat cur-heat
                   :specials/abilities special-list
                   :damageable/crit-list crit-list
                   ;; TODO Replace for transition to GTK
                   :display/image-path (namestring asset-path)
                   :location/q q
                   :location/r r
                   :location/s s
                   :pilot/name pilot
                   :pilot/skill skill)))

(defmethod same-entity ((e entity) (o entity))
  (eq (entity-id e) (entity-id o)))

(defmethod select ((e combat-unit))
  (if (or (not (game/initiative-list *game*))
          (< (length (game/initiative-list *game*)) (game/initiative-place *game*)))
      (progn
        (run-clear-selection)
        (setf (can-activate/selectedp e) t)
        (setf (game/active-unit *game*) e))
      (if (and (not (can-activate/has-acted e))
               (same-force (info/force e) (nth (game/initiative-place *game*)
                                             (game/initiative-list *game*))))
          (progn
            (run-clear-selection)
            (setf (can-activate/selectedp e) t)
            (setf (game/active-unit *game*) e)))))

;; (define-presentation-method present (combat-unit
;;                                      (type entity)
;;                                      stream
;;                                      (view graphical-view) &key)
;;   (let ((origin (hex-to-pixel (new-hexagon :q (location/q combat-unit)
;;                                            :r (location/r combat-unit)
;;                                            :s (location/s combat-unit))
;;                               (frame/layout *application-frame*)))
;;         (color (force/color (info/force combat-unit)))
;;         (layout (frame/layout *application-frame*)))
;;     (with-translation (stream (* (layout-x-size layout) -0.9)
;;                               (* (layout-y-size layout) -0.8))
;;       (draw-pattern* stream (display/image-path combat-unit)
;;                      (point-x origin) (point-y origin)))
;;     (with-translation (stream 0 (* (layout-y-size layout) -0.8))
;;       (surrounding-output-with-border (stream :ink color :filled t :shape :rectangle)
;;         (if (can-activate/selectedp combat-unit)
;;             (with-text-style (stream *selected-text-style*)
;;               (draw-text stream (format nil "~a" (info/short-name combat-unit))
;;                          origin :align-x :center :align-y :top))
;;             (draw-text stream (format nil "~a" (info/short-name combat-unit))
;;                        origin :align-x :center :align-y :top))))))

(defmethod get-hex ((cu combat-unit) (g board))
  (gethash (offset-from-hex (new-hexagon :q (location/q cu)
                                         :r (location/r cu)
                                         :s (location/s cu)))
           (board/tiles g)))

;; (define-presentation-method present (combat-unit
;;                                      (type entity)
;;                                      stream
;;                                      (view quickstats-view) &key)
;;   (quickstats-block stream combat-unit))

;; (define-presentation-method present (combat-unit
;;                                      (type entity)
;;                                      stream
;;                                      (view textual-view) &key)
;;   (format stream "~a #~a" (info/full-name combat-unit) (entity-id combat-unit)))
;;; Movement methods

(defun format-move-assoc (stream m colonp atsignp)
  "The colonp and atsignp are required for a function called inside `FORMAT'."
  (declare (ignorable colonp atsignp))
  (format stream "~a~a" (cdr m) (cdr (assoc (car m) *mv-designators*))))

(defmethod moveable/format-move ((m moveable))
  (format nil "~{~/megastrike::format-move-assoc/~^/~}" (moveable/move-alist m)))

(defmethod pilot/display ((p pilot))
  (format nil "~a (~a)" (pilot/name p) (pilot/skill p)))

(defmethod move-lookup ((m moveable) (mv-type symbol))
  (cdr (assoc mv-type (moveable/move-alist m))))

(defmethod move-unit ((unit combat-unit) (destination tile))
  (if (>= (move-lookup unit (moveable/move-used unit))
          (hex-distance (new-hexagon :q (location/q unit)
                                     :r (location/r unit)
                                     :s (location/s unit))
                        destination))
      (progn (set-location unit destination)
             (incf (game/initiative-place *game*))
             (setf (can-activate/has-acted unit) t)
             (setf (game/phase-log *game*)
                   (concatenate 'string (game/phase-log *game*)
                                (format nil "~a has moved to ~a.~%"
                                        (info/full-name unit)
                                        (offset-from-hex destination)))))))

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
        (to-hit (roll2d))
        (log-string ""))
    (setf log-string (concatenate 'string log-string
                                  (format nil "~a attacking ~a (needs a ~d): Rolled ~d~%"
                                          (info/full-name attacker)
                                          (info/full-name target)
                                          target-num
                                          to-hit)))
    (if (<= target-num to-hit)
        (take-damage target (calculate-damage attacker target))))
  (incf (game/initiative-place *game*))
  (setf (can-activate/has-acted attacker) t))

(defmethod take-damage ((u damageable) damage)
  (dotimes (x damage)
    (if (eq 0 (damageable/cur-armor u))
        (decf (damageable/cur-struct u))
        (decf (damageable/cur-armor u))))
  (if (>= 0 (damageable/cur-struct u))
      (setf (damageable/destroyedp u) t))
  (setf (game/phase-log *game*)
        (concatenate 'string (game/phase-log *game*)
                     (format nil "~a now has ~a armor and ~a structure.~%"
                             (info/full-name u)
                             (damageable/cur-armor u)
                             (damageable/cur-struct u)))))
