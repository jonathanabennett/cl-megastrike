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

(defparameter *here* (asdf:system-source-directory :megastrike))
(defparameter *data-folder* (merge-pathnames "data/" *here*))

(defun build-mechset ()
  (let ((f (uiop:read-file-lines (merge-pathnames "images/units/mechset.txt"
                                                  *data-folder*)))
        (collection '()))
    (loop for line in f
          do (let ((row (parse-mechset-line line)))
               (when row
                 (push row collection))))
    collection))

(defun parse-mechset-line (line)
  (unless (or (equal (search "#" line) 0)
              (string= "" line)
              (equal (search "include" line) 0))
    (let* ((mechset '())
         (first-break (search " " line))
         (second-break (search "\" " line :start2 (1+ first-break))))
      (push (string-trim "\" " (subseq line second-break)) mechset)
      (push (string-trim "\" " (subseq line first-break second-break)) mechset)
      (push (string-trim "\" " (subseq line 0 first-break)) mechset)
      mechset)))

(defvar *mechset* (build-mechset))

(defvar *game*)
(defvar *lobby*)

(defmacro add-to-end (target item)
  "This macro cleans up calls to append to the end of lists, something I need to do a lot in this codebase."
  `(setf ,target (append ,target (list ,item))))

(cl:defpackage cairo-gobject
  (:use)
  (:export #:*ns*))

(cl:in-package #:megastrike)

(gir-wrapper:define-gir-namespace "cairo")

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
