(in-package :alphastrike)


(defun combat-unit-id (combat-unit)
  (let ((id-string (random-uuid:to-string (id combat-unit))))
    (first (cl-ppcre:split "(-)" id-string :limit 2))))

(defun quickstats-block (stream combat-unit)
  "Draws the quick-stats block for a unit."
  (let ((element (element combat-unit)))
    (surrounding-output-with-border (stream :ink +light-gray+ :filled t :shape :rounded)
      (with-text-style (stream (make-text-style :serif :bold :normal))
        (format stream "~a " (name element)))
      (format stream "#~a " (combat-unit-id combat-unit))
      (format stream "~a~%" (kind element))
      (format stream "  A/S: ~a/~a" (current-armor element) (current-struct element))
      (format stream "  MV: ~a" (format-move element))
      (format stream "  TMM: ~a~%" (unit-tmm combat-unit)))))

(defun general-info-block (stream combat-unit)
  "Draws the first block of the Record sheet, containing the Type, MV, Role, and Pilot info."
  (let ((element (element combat-unit)))
    (surrounding-output-with-border (stream :ink +light-gray+ :filled t :shape :rounded)
      (with-text-style (stream (make-text-style :serif :bold :normal))
        (format stream "TP: "))
        (format stream "~A " (kind element))
      (with-text-style (stream (make-text-style :serif :bold :normal))
        (format stream "MV: "))
        (format stream "~A " (format-move element))
      (with-text-style (stream (make-text-style :serif :bold :normal))
        (format stream "Role: "))
        (format stream "~a" (role element))
        (format stream "~%")
      (with-text-style (stream (make-text-style :serif :bold :normal))
        (format stream "Pilot info: "))
        (format stream "~A" (display (pilot combat-unit))))))

(defun attack-info-block (stream combat-unit)
  "Draws the attack and heat information."
  (let ((element (element combat-unit)))
    (surrounding-output-with-border (stream :ink +light-gray+ :filled t :shape :rounded)
      (dolist (atk (attack-list element))
        (with-text-style (stream (make-text-style :serif :bold :normal))
          (format stream "Attack: "))
        (format stream "~a: " (kind atk))
        (format stream "~{~a~}~%" (attack atk)))
      (with-text-style (stream (make-text-style :serif :bold :normal))
        (format stream "OV: "))
      (format stream "~a  " (overheat element))
      (dotimes (box 5)
        (if (<= box (heat element))
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
              (format stream "~a " box)))))))

(defun damage-info-block (stream combat-unit)
  "Draws the current damage and structure and any critical hits."
  (let ((element (element combat-unit)))
    (surrounding-output-with-border (stream :ink +light-gray+ :filled t :shape :rounded)
      (with-text-style (stream (make-text-style :serif :bold :normal))
        (format stream "Armor: "))
      (dotimes (pip (armor element))
        (if (< pip (current-armor element))
            (surrounding-output-with-border (stream
                                            :shape :rectangle
                                            :move-cursor nil)
              (format stream " ~a " (+ pip 1)))
            (surrounding-output-with-border (stream
                                            :filled t
                                            :shape :rectangle
                                            :move-cursor nil)
              (format stream " ~a " (+ pip 1))))
        (format stream "  "))
      (with-text-style (stream (make-text-style :serif :bold :normal))
        (format stream "Struct: "))
      (dotimes (pip (struct element))
        (if (< pip (current-struct element))
            (surrounding-output-with-border (stream
                                            :shape :rectangle
                                            :move-cursor nil)
              (format stream " ~a " (+ pip 1)))
            (surrounding-output-with-border (stream
                                            :filled t
                                            :shape :rectangle
                                            :move-cursor nil)
              (format stream " ~a " (+ pip 1))))
        (format stream "  ")))))

(defun specials-info-block (stream combat-unit)
  (let ((element (element combat-unit)))
    "Draws the Specials"))
