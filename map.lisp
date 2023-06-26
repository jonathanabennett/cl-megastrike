(in-package #:alphastrike)

;; Begin by defining a Hexagon class. We are using the Cubic constructor and
;; storage method described by Amin at Red Blob Games.
(defclass hexagon ()
  ((q
    :initarg :q
    :accessor q
    :documentation "location on the Q axis.")
   (r
    :initarg :r
    :accessor r
    :documentation "location on the R axis.")
   (s
    :initarg :s
    :accessor s
    :documentation "location on the S axis.")))

(defun make-hexagon (&key q r s)
  (if (eq (+ q r s) 0)
      (make-instance 'hexagon
                     :q q
                     :r r
                     :s s)))


(defun hex-from-offset (&key col row)
  "Creates hexes from the .board files used by MegaMek, which use an offset coordinate system instead of cubic like I do."
  (let ((q col)
        (- row (floor (/ (+ col (* (bit-and col) -1)) 2)))
        (s (+ (* q -1) r))))
  (make-hexagon :q q :r r :s s))

(defmethod offset-from-hex ((hex hexagon))
  (let ((row (+ (hex r) (floor (/ (+ (hex q) (* (bit-and (hex q)) -1))))))
        (col (hex q)))
    (list col row)))

(defmethod same-hex ((hex1 hexagon) (hex2 hexagon))
  "Hexagons are equal if their q, r, and s coordinates are the same."
  (and (= (s hex1) (s hex2))
       (= (r hex1) (r hex2))
       (= (q hex1) (q hex2))))

(defmethod hex-addition ((a hexagon) (b hexagon))
  "Uses Cartesian addition to add two hexagons together."
  (make-hexagon :q (+ (q a) (q b))
                :r (+ (r a) (r b))
                :s (+ (s a) (s b))))

(defmethod hex-subtract ((a hexagon) (b hexagon))
  "Uses Cartesian subtraction to add two hexagons together."
  (make-hexagon :q (- (q a) (q b))
                :r (- (r a) (r b))
                :s (- (s a) (s b))))

(defmethod hex-multiply ((a hexagon) x)
  "Uses Cartesian multiplication to add two hexagons together."
  (make-hexagon :q (* (q a) x)
                :r (* (r a) x)
                :s (* (s a) x)))

(defun hex-length (hex)
  "The length of the distance between two hexagons is calculated similarly to
Manhattan distances with a square grid, but you half the sum to get the final
distance. The function cannot be called without hex-distance to produce the
appropriate hexagon first."
  (/ (+ (abs (q hex)) (abs (r hex)) (abs (s hex))) 2))

(defmethod hex-distance ((a hexagon) (b hexagon))
  "This produces the hex we use in hex-length."
  (hex-length (hex-subtract a b)))

;; We use the following Vector to calculate neighbors.
(defvar *hex-directions* (vector (make-hexagon :q 1 :r 0 :s -1)
                                 (make-hexagon :q 1 :r -1 :s 0)
                                 (make-hexagon :q 0 :r -1 :s 1)
                                 (make-hexagon :q -1 :r 0 :s 1)
                                 (make-hexagon :q -1 :r 1 :s 0)
                                 (make-hexagon :q 0 :r 1 :s -1))
  "This vector describes the offsets to calculate neighbors.")

(defun hex-direction (direction)
  "Return a hexagon representing the change in q,r,s to move in a given direction."
  (if (and (<= 0 direction) (> 6 direction))
      (elt *hex-directions* direction)))

(defmethod hex-neighbor ((hex hexagon) direction)
  "Return the hex which is the neighbor in that direction."
  (hex-addition hex (hex-direction direction)))

(defclass hex-point ()
  ((x
    :initarg :x
    :accessor x)
   (y
    :initarg :y
    :accessor y)))

(defun make-hex-point (&key x y)
  (make-instance 'hex-point :x x :y y))

;; Display Implementation

(defstruct layout
  hex-to-pixel-matrix
  pixel-to-hex-matrix
  start-angle
  x-size
  y-size
  x-origin
  y-origin)

;; This function does the math to convert from a q,r,s address to an x,y
;; position for the center fo the hex.

(defun hex-to-pixel (hex layout)
  (let ((vec (layout-hex-to-pixel-matrix layout)))
    (make-hex-point :x (+ (* (+ (* (elt vec 0) (q hex))
                            (* (elt vec 1) (r hex)))
                         (layout-x-size layout))
                      (layout-x-origin layout))
                :y (+ (* (+ (* (elt vec 2) (q hex))
                            (* (elt vec 3) (r hex)))
                         (layout-y-size layout))
                      (layout-y-origin layout)))))

(defun pixel-to-hex (mouse-hex-point layout)
  (let* ((vec (layout-pixel-to-hex-matrix layout))
         (modified-hex-point (make-hex-point :x (/ (- (x mouse-hex-point) (layout-x-origin layout)) (layout-x-size layout))
                                     :y (/ (- (x mouse-hex-point) (layout-y-origin layout)) (layout-y-size layout))))
         (calc-q (+ (* (elt vec 0) (x modified-hex-point)) (* (elt vec 1) (y modified-hex-point))))
         (calc-r (+ (* (elt vec 2) (x modified-hex-point)) (* (elt vec 3) (y modified-hex-point)))))
    (make-hexagon :q calc-q :r calc-r :s (+ (* calc-q -1) (* calc-r -1)))))

(defun find-hex-corner (center corner layout)
  (let ((angle (* 2.0 pi (/ (+ (layout-start-angle layout)
                               corner) 6))))
    (make-hex-point :x (+ (* (layout-x-size layout)
                         (cos angle))
                      (x center))
                :y (+ (* (layout-y-size layout)
                         (sin angle))
                      (y center)))))

(defun hex-point-to-list (hex-point)
  (list (x hex-point) (y hex-point)))

(defun hex-point-to-clim-point (hex-point)
  (make-point (x hex-point) (y hex-point)))

(defun draw-hex (hex layout)
  (let ((center (hex-to-pixel hex layout))
        (hex-points '()))
    (dotimes (i 6)
      (push (hex-point-to-clim-point (find-hex-corner center i layout)) hex-points))
     hex-points))

(defun pixel-to-hex (origin-hex-point hex-point-size hex-point)
  "Not yet implemented.")
