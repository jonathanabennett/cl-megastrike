(in-package :alphastrike)


(defun quickstats-block (stream combat-unit)
  "Draws the quick-stats block for a unit."
  (surrounding-output-with-border (stream :ink +light-gray+ :filled t :shape :rounded)
    (with-text-style (stream (make-text-style :serif :bold :normal))
      (format stream "~a ~a" (info/full-name combat-unit) (info/short-name combat-unit)))
    (format stream "#~a " (entity-id combat-unit))
    (format stream "~a~%" (info/unit-type combat-unit))
    (format stream "  A/S: ~a/~a" (damageable/cur-armor combat-unit)
                                  (damageable/cur-struct combat-unit))
    (format stream "  MV: ~a" (format-move combat-unit))
    (format stream "  TMM: ~a~%" (unit-tmm combat-unit))))

(defun general-info-block (stream combat-unit)
  "Draws the first block of the Record sheet, containing the Type, MV, Role, and Pilot info."
  (surrounding-output-with-border (stream :ink +light-gray+ :filled t :shape :rounded)
    (with-text-style (stream (make-text-style :serif :bold :normal))
      (format stream "TP: "))
      (format stream "~A " (info/unit-type combat-unit))
    (with-text-style (stream (make-text-style :serif :bold :normal))
      (format stream "MV: "))
      (format stream "~A " (format-move combat-unit))
    (with-text-style (stream (make-text-style :serif :bold :normal))
      (format stream "Role: "))
      (format stream "~a" (info/role combat-unit))
      (format stream "~%")
    (with-text-style (stream (make-text-style :serif :bold :normal))
      (format stream "Pilot info: "))
      (format stream "~A ~A" (pilot/name combat-unit) (pilot/skill combat-unit))))

(defun attack-info-block (stream combat-unit)
  "Draws the attack and heat information."
  (surrounding-output-with-border (stream :ink +light-gray+ :filled t :shape :rounded)
      (with-text-style (stream (make-text-style :serif :bold :normal))
        (format stream "Attack: "))
    (format stream "S: ~a M: ~a L: ~a" (attacks/short combat-unit)
                                       (attacks/medium combat-unit)
                                       (attacks/long combat-unit)))
    (with-text-style (stream (make-text-style :serif :bold :normal))
      (format stream "OV: "))
    (format stream "~a  " (heat/ov combat-unit))
    (dotimes (box 5)
      (if (<= box (heat/cur-heat combat-unit))
          (surrounding-output-with-border (stream
                                            :ink +red+
                                            :filled t
                                            :shape :rectangle
                                            :move-cursor nil)
            (format stream "~a " box))
          (surrounding-output-with-border (stream
                                            :ink +light-pink+
                                            :filled t
                                            :shape :rectangle
                                            :move-cursor nil)
            (format stream "~a " box)))))

(defun damage-info-block (stream combat-unit)
  "Draws the current damage and structure and any critical hits."
  (surrounding-output-with-border (stream :ink +light-gray+ :filled t :shape :rounded)
    (with-text-style (stream (make-text-style :serif :bold :normal))
      (format stream "Armor: "))
    (dotimes (pip (damageable/max-armor combat-unit))
      (if (< pip (damageable/cur-armor combat-unit))
          (surrounding-output-with-border (stream
                                          :shape :rectangle
                                          :move-cursor nil)
            (format stream " ~a " (+ pip 1)))
          (surrounding-output-with-border (stream
                                          :filled t
                                          :ink +red+
                                          :shape :rectangle
                                          :move-cursor nil)
            (format stream " ~a " (+ pip 1))))
      (format stream "  "))
    (with-text-style (stream (make-text-style :serif :bold :normal))
      (format stream "Struct: "))
    (dotimes (pip (damageable/max-struct combat-unit))
      (if (< pip (damageable/cur-struct combat-unit))
          (surrounding-output-with-border (stream
                                          :shape :rectangle
                                          :move-cursor nil)
            (format stream " ~a " (+ pip 1)))
          (surrounding-output-with-border (stream
                                          :filled t
                                          :ink +red+
                                          :shape :rectangle
                                          :move-cursor nil)
            (format stream " ~a " (+ pip 1))))
      (format stream "  "))))

;; (defun specials-info-block (stream combat-unit)
;;   "Draws the Specials")
