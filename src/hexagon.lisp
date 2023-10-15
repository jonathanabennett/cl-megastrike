(in-package :megastrike)

;; Begin by defining a Hexagon class. We are using the Cubic constructor and
;; storage method described by Amin at Red Blob Games.

;; Replace this with a standard class
;; Prefix all accessors with `hexagon-'
;; Convert defuns to defmethods
(defclass hexagon ()
  ((q :initarg :q :accessor hexagon-q)
   (r :initarg :r :accessor hexagon-r)
   (s :initarg :s :accessor hexagon-s)))

(defun new-hexagon (&key q r s)
  (if (eq 0 (+ q r s))
      (make-instance 'hexagon :q q :r r :s s)
      (make-instance 'hexagon :q q :r r :s (* (+ q r) -1))))

;; Code to convert from qrs to xy and back again.
(defun hex-from-offset (&key col row)
  "Creates hexes from the .board files used by MegaMek,
which use an offset coordinate system instead of cubic like I do."
  (let* ((q col)
         (r (- row (floor (/ (+ col (* (mod (abs col) 2) -1)) 2))))
         (s (* (+ q r) -1)))
    (if (eq (+ q r s) 0)
        (new-hexagon :q q :r r :s s))))


(defmethod offset-from-hex ((hex hexagon))
  "Creates xy coordinates from cubic coordinates using a hexagon."
  (let ((row (+ (hexagon-r hex) (floor (/ (+ (hexagon-q hex) (* (mod (abs (hexagon-q hex)) 2) -1)) 2))))
        (col (hexagon-q hex)))
    (list col row)))

(defmethod same-hex ((hex1 hexagon) (hex2 hexagon))
  "Hexagons are equal if their q, r, and s coordinates are the same."
  (and (= (hexagon-s hex1) (hexagon-s hex2))
       (= (hexagon-r hex1) (hexagon-r hex2))
       (= (hexagon-q hex1) (hexagon-q hex2))))

(defmethod same-hex (hex1 (hex2 hexagon))
  "If one of the two things is not a hex, they are not equal."
  nil)

(defmethod same-hex ((hex1 hexagon) hex2)
  "If one of the two things is not a hex, they are not equal."
  nil)

(defmethod hex-addition ((hex1 hexagon) (hex2 hexagon))
  "Uses Cartesian addition to add two hexagons together."
  (new-hexagon :q (+ (hexagon-q hex1) (hexagon-q hex2))
               :r (+ (hexagon-r hex1) (hexagon-r hex2))
               :s (+ (hexagon-s hex1) (hexagon-s hex2))))

(defmethod hex-subtract ((hex1 hexagon) (hex2 hexagon))
  "Uses Cartesian subtraction to subtract hexagon b from hexagon a."
  (new-hexagon :q (- (hexagon-q hex1) (hexagon-q hex2))
               :r (- (hexagon-r hex1) (hexagon-r hex2))
               :s (- (hexagon-s hex1) (hexagon-s hex2))))

(defmethod hex-multiply ((hex hexagon) x)
  "Uses Cartesian multiplication to multiply a hex by a value x together."
  (new-hexagon :q (* (hexagon-q hex) x)
               :r (* (hexagon-r hex) x)
               :s (* (hexagon-s hex) x)))


(defmethod hex-distance ((hex1 hexagon) (hex2 hexagon))
  "The length of the distance between two hexagons is calculated similarly to
Manhattan distances with a square grid, but you half the sum to get the final
distance."
  (let ((hex-length (hex-subtract hex1 hex2)))
    (/ (+ (abs (hexagon-q hex-length)) (abs (hexagon-r hex-length)) (abs (hexagon-s hex-length))) 2)))

(defvar *hex-directions* (vector (new-hexagon :q  1  :r  0 :s -1)
                                 (new-hexagon :q  1  :r -1 :s  0)
                                 (new-hexagon :q  0  :r -1 :s  1)
                                 (new-hexagon :q -1  :r  0 :s  1)
                                 (new-hexagon :q -1  :r  1 :s  0)
                                 (new-hexagon :q  0  :r  1 :s -1))
  "This vector describes the offsets to calculate neighbors.")

(defun hex-direction (direction)
  "Returns the coordinate transformation to select a hex in a given direction."
  (if (and (<= 0 direction) (> 6 direction))
      (elt *hex-directions* direction)))

(defmethod hex-neighbor ((hex hexagon) direction)
  "Finds the neighbor of a `hex' in a given `direction'.
`direction' must be an integer between 0 and 5 inclusive."
  (hex-addition hex (hex-direction direction)))

;; Display Implementation
(defstruct layout
  hex-to-pixel-matrix
  pixel-to-hex-matrix
  start-angle
  x-size
  y-size
  x-origin
  y-origin)

(defparameter +default-layout+ (make-layout
                                :hex-to-pixel-matrix
                                  (vector (/ 3.0 2.0) 0 (/ (sqrt 3.0) 2.0) (sqrt 3.0))
                                :pixel-to-hex-matrix
                                  (vector (/ 2.0 3.0) 0 (/ -1.0 3.0) (/ (sqrt 3.0) 3.0))
                                :start-angle 0
                                :x-size 70
                                :y-size 70
                                :x-origin 20
                                :y-origin 20))

(defclass point ()
  ((x :initarg :x
      :accessor point-x)
   (y :initarg :y
      :accessor point-y)))

(defun make-point (x y)
  (make-instance 'point :x x :y y))

(defmethod hex-to-pixel ((hex hexagon) layout)
  "Converts from a q,r,s address, to an x,y pixel position for the center of the hex."
  (let ((vec (layout-hex-to-pixel-matrix layout)))
    (make-point (+ (* (+ (* (elt vec 0) (hexagon-q hex))
                            (* (elt vec 1) (hexagon-r hex)))
                         (layout-x-size layout))
                      (layout-x-origin layout))
                (+ (* (+ (* (elt vec 2) (hexagon-q hex))
                            (* (elt vec 3) (hexagon-r hex)))
                         (layout-y-size layout))
                      (layout-y-origin layout)))))

(defun pixel-to-hex (pt layout)
  "Converts from an x,y pixel position to a q,r,s address."
  (let ((vec (layout-pixel-to-hex-matrix layout))
        (modified-point nil)
        (q nil) (r nil))
    (setf modified-point (make-point
                          (/ (- (point-x pt)
                                (layout-x-origin layout))
                             (layout-x-size layout))
                          (/ (- (point-y pt)
                                (layout-y-origin layout))
                             (layout-y-size layout))))
    (format t "Modified point: ~a, ~a~%" (point-x modified-point) (point-y modified-point))
    (setf q (+ (* (point-x modified-point) (elt vec 0))
               (* (point-y modified-point) (elt vec 1))))
    (setf r (+ (* (point-x modified-point) (elt vec 2))
               (* (point-y modified-point) (elt vec 3))))
    (format t "Hex Address: ~a, ~a, ~a~%" q r (* (+ q r) -1))
    (hex-round q r (* (+ q r) -1))))

(defun find-hex-corner (center corner layout)
  "Find the x,y pixel coordinates for the corner of a hex."
  (let ((angle (* 2.0 pi (/ (+ (layout-start-angle layout)
                               corner) 6))))
    (make-point (+ (* (layout-x-size layout)
                         (cos angle))
                      (point-x center))
                (+ (* (layout-y-size layout)
                         (sin angle))
                      (point-y center)))))

(defmethod draw-hex ((hex hexagon) layout)
  (let ((center (hex-to-pixel hex layout))
        (points '()))
    (dotimes (i 6)
      (push (find-hex-corner center i layout) points))
     points))

(defun hex-round (q r s)
  (let* ((q-int (round q))
         (r-int (round r))
         (s-int (round s))
         (q-diff (abs (- q q-int)))
         (r-diff (abs (- r r-int)))
         (s-diff (abs (- s s-int))))
    (cond
      ((and (> q-diff r-diff)
            (> q-diff s-diff))
       (new-hexagon :q (* (+ r-int s-int) -1) :r r-int :s s-int))
      ((and (> r-diff s-diff)
            (> r-diff q-diff))
       (new-hexagon :q q-int :r (* (+ q-int s-int) -1) :s s-int))
      (t (new-hexagon :q q-int :r r-int :s (* (+ q-int r-int) -1))))))

(defun linear-interpolation (a b step)
  (+ a (* (- b a) step)))

(defmethod hex-lerp ((a hexagon) (b hexagon) step)
  (let ((q (linear-interpolation (hexagon-q a) (hexagon-q b) step))
        (r (linear-interpolation (hexagon-r a) (hexagon-r b) step))
        (s (linear-interpolation (hexagon-s a) (hexagon-s b) step))))
  (values q r s))

(defmethod hex-line ((a hexagon) (b hexagon))
  (let* ((distance (hex-distance a b))
         (hex-list '())
         (step (/ 1.0 (max distance 1))))
    (dotimes (c distance)
      (push (hex-round (hex-lerp a b (* c step))) hex-list))
    hex-list))
