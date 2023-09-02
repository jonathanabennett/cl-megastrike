(in-package :megastrike)

;;; Define globals and major variables
;;;

(deftype elements-type ()
  "This defines the valid list of element types in the game."
  '(member BM IM PM SV CV BA CI SS WS JS DS DA SC CF AF))

(defparameter +ALL-TYPES+ '("BM" "IM" "PM" "SV" "CV" "BA" "CI" "SS"
                            "WS" "JS" "DS" "DA" "SC" "CF" "AF"))

(defparameter +GROUND-UNITS+ '("BM" "IM" "PM" "SV" "CV" "BA" "CI"))
(defparameter +AERO-UNITS+ '("SS" "WS" "JS" "DS" "DA" "SC" "CF" "AF"))
(defparameter +BM-UNITS+ '("BM"))
(defparameter +MECH-UNITS+ '("BM" "IM" "PM"))
(defparameter +CONVENTIONAL-UNITS+ '("SV" "CV" "BA" "CI"))
(defparameter +VEHICLE-UNITS+ '("SV" "CV"))
(defparameter +INFANTRY-UNITS+ '("BA" "CI"))

(defvar *mv-designators* '((:walk            . "")
                           (:jump            . "j")
                           (:tracked-quadvee . "qt")
                           (:wheeled-quadvee . "qw")
                           (:tracked         . "t")
                           (:wheeled         . "w")
                           (:hover           . "h")
                           (:vtol            . "v")
                           (:naval           . "n")
                           (:submarine       . "s")
                           (:foot-inf        . "f")
                           (:mot-inf         . "m")
                           (:wige            . "g")
                           (:aerodyne        . "a")
                           (:spheroid        . "p")
                           ))

(deftype crit ()
  "This defines the possible critical hits an element can take in the game."
  '(member 'ENGINE 'FIRE-CONTROL 'MP 'WEAPONS))


(deftype phase-type ()
  '(member initiative deployment movement combat end))

(defvar *phase-order* '(:initiative :deployment :movement :combat :end))

(defun roll (dice &optional (mods 0))
  (let ((ret mods))
    (dotimes (x dice) (incf ret (+ (random 6) 1)))
    ret))

(defun roll2d (&optional (mods 0))
  (roll 2 mods))

(defparameter *here* (uiop:pathname-parent-directory-pathname (uiop:getcwd)))

(defvar *game*)
(defvar *lobby*)
(defvar *current-layout*)
