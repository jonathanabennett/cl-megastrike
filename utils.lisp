(in-package :alphastrike)

;;; Define globals and major variables
;;;


(deftype elements-type ()
  "This defines the valid list of element types in the game."
  '(member BM DS))
(deftype move-type ()
  "This defines the valid ways an element can move in the game."
  '(member walk jump))
(defvar *mv-designators* '((walk . "")
                           (jump . "j")))
(deftype crit ()
  "This defines the possible critical hits an element can take in the game."
  '(member 'ENGINE 'FIRE-CONTROL 'MP 'WEAPONS))


(defclass graphical-view (view)
  ())

(defclass quickstats-view (view)
  ())

(defparameter +graphical-view+ (make-instance 'graphical-view))
(defparameter +quickstats-view+ (make-instance 'quickstats-view))

(deftype phase-type ()
  '(member initiative deployment movement combat end))

(defvar *phase-order* '(:initiative :deployment :movement :combat :end))
