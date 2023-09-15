(cl:defpackage cairo-gobject
  (:use)
  (:export #:*ns*))

(cl:in-package #:megastrike)

(gir-wrapper:define-gir-namespace "cairo")

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

(defparameter *mv-designators* '((:walk            . "")
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
                                (:spheroid        . "p")))

(defparameter pv-skill-ratings '((7 . 0.68)
                                 (6 . 0.77)
                                 (5 . 0.86)
                                 (4 . 1.00)
                                 (3 . 1.38)
                                 (2 . 1.82)
                                 (1 . 2.24)
                                 (0 . 2.63)))

(defparameter skill-names '((7 . "Wet Behind the Ears")
                            (6 . "Really Green")
                            (5 . "Green")
                            (4 . "Regular")
                            (3 . "Veteran")
                            (2 . "Elite")
                            (1 . "Heroic")
                            (0 . "Legendary")
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

(defmacro add-to-end (target item)
  "This macro cleans up calls to append to the end of lists, something I need to do a lot in this codebase."
  `(setf ,target (append ,target (list ,item))))

(cffi:defcstruct gdk-rgba
  (red :double)
  (green :double)
  (blue :double)
  (alpha :double))

(defmacro with-gdk-rgba ((pointer color) &body body)
  `(locally
       #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
       (cffi:with-foreign-object (,pointer '(:struct gdk-rgba))
         (let ((,pointer (make-instance 'gir::struct-instance
                                        :class (gir:nget gdk::*ns* "RGBA")
                                        :this ,pointer)))
           (gdk:rgba-parse ,pointer ,color)
           (locally
               #+sbcl (declare (sb-ext:unmuffle-conditions sb-ext:compiler-note))
               ,@body)))))

(declaim (ftype (function (t t t t) t) draw-func))

(cffi:defcallback %draw-func :void ((area :pointer)
                                    (cr :pointer)
                                    (width :int)
                                    (height :int)
                                    (data :pointer))
  (declare (ignore data))
  (let ((cairo:*context* (make-instance 'cairo:context
                                        :pointer cr
                                        :width width
                                        :height height
                                        :pixel-based-p nil)))
    (draw-func (make-instance 'gir::object-instance
                              :class (gir:nget gtk:*ns* "DrawingArea")
                              :this area)
               (make-instance 'gir::struct-instance
                              :class (gir:nget megastrike::*ns* "Context")
                              :this cr)
               width height)))
