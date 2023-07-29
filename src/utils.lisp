(in-package :megastrike)

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

(defun roll (dice &optional (mods 0))
  (let ((ret mods))
    (dotimes (x dice) (incf ret (+ (random 6) 1)))
    ret))

(defun roll2d (&optional (mods 0))
  (roll 2 mods))

(defparameter *selected-text-style* (make-text-style :serif :bold :normal))

(defparameter *here* (uiop:pathname-parent-directory-pathname (uiop:getcwd)))

(defvar *game*)
(defvar *lobby*)
