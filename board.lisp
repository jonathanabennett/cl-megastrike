(in-package #:alphastrike)

(defun parse-hex-address (str)
  (let ((x (parse-integer (subseq str 0 2)))
        (y (parse-integer (subseq str 2 4))))
    (list x y)))

(defun parse-hex-line (line)
  (let* ((line-string (cl-ppcre:split "( )" line))
         (hex-address (nth 1 line-string))
         (elevation   (nth 2 line-string))
         (terrain     (nth 3 line-string))
         (style       (nth 4 line-string)))
    (list hex-address elevation terrain style)))

(defclass grid ()
  ((tile-hash
    :initarg :tiles
    :accessor tiles
    :documentation "The hash of the Tile objects which make up the map, stored by xy coordinates.")
   (units
    :initarg units
    :accessor units
    :documentation "A hash of the Unit objects on the map, stored by xy coordinates.")))

(defstruct tile
  elevation terrain depth)

;; Code to convert from qrs to xy and back again.
;; Note that &1 is bitwise and, how do you do that in Common Lisp?
;; function axial-to-oddq(hex):
;;   var col = hex.q
;;   var row = hex.r + (hex.q - (hex.q&1))/2
;;   return point(col, rol)

;; function oddq-to-axial(hex):
;;   var q = hex.col
;;   var r = hex.row - (hex.col - (hex.col&1)) / 2
;;   return Hex(q, r)

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
  "Uses Cartesian subtraction to subtract hexagon b from hexagon a."
  (make-hexagon :q (- (q a) (q b))
                :r (- (r a) (r b))
                :s (- (s a) (s b))))

(defmethod hex-multiply ((a hexagon) x)
  "Uses Cartesian multiplication to multiply two hexagons together."
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
  (if (and (<= 0 direction) (> 6 direction))
      (elt *hex-directions* direction)))

(defmethod hex-neighbor ((hex hexagon) direction)
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

;; This function does the math to convert from a q,r,s address to an x,y
;; position for the center fo the hex.

(defun hex-to-pixel (hex layout)
  (let ((vec (layout-hex-to-pixel-matrix layout)))
    (make-point (+ (* (+ (* (elt vec 0) (q hex))
                            (* (elt vec 1) (r hex)))
                         (layout-x-size layout))
                      (layout-x-origin layout))
                (+ (* (+ (* (elt vec 2) (q hex))
                            (* (elt vec 3) (r hex)))
                         (layout-y-size layout))
                      (layout-y-origin layout)))))

(defun pixel-to-hex (mouse-point layout)
  (let* ((vec (layout-pixel-to-hex-matrix layout))
         (modified-point (make-point (/ (- (point-x mouse-point) (layout-x-origin layout)) (layout-x-size layout))
                                     (/ (- (point-y mouse-point) (layout-y-origin layout)) (layout-y-size layout))))
         (calc-q (+ (* (elt vec 0) (point-x modified-point)) (* (elt vec 1) (point-y modified-point))))
         (calc-r (+ (* (elt vec 2) (point-x modified-point)) (* (elt vec 3) (point-y modified-point)))))
    (make-hexagon :q calc-q :r calc-r :s (+ (* calc-q -1) (* calc-r -1)))))

(defun find-hex-corner (center corner layout)
  (let ((angle (* 2.0 pi (/ (+ (layout-start-angle layout)
                               corner) 6))))
    (make-point (+ (* (layout-x-size layout)
                         (cos angle))
                      (point-x center))
                (+ (* (layout-y-size layout)
                         (sin angle))
                      (point-y center)))))

(defun point-to-list (point)
  (list (point-x point) (point-y point)))

(defun draw-hex (hex layout)
  (let ((center (hex-to-pixel hex layout))
        (points '()))
    (dotimes (i 6)
      (push (find-hex-corner center i layout) points))
     points))

(defun pixel-to-hex (origin-point point-size point)
  "Not yet implemented.")
