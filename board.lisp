(in-package #:alphastrike)

(defun parse-hex-address (str)
"A hex address from a board file is a string in the format xxyy"
  (let ((x (parse-integer (subseq str 0 2)))
        (y (parse-integer (subseq str 2 4))))
    (list x y)))

(defun parse-hex-line (line)
  "A line from a .board file looks like this:

hex xxyy elevation terrain style

For example:

hex 0101 3 \"woods:1;foliage_elev:2\" \"grass\""
  (let* ((line-string (cl-ppcre:split "( )" line))
         (hex-address (nth 1 line-string))
         (elevation   (nth 2 line-string))
         (terrain     (nth 3 line-string))
         (style       (nth 4 line-string)))
    (list hex-address elevation terrain style)))

;; (defclass grid ()
;;   ((tile-hash
;;     :initarg :tiles
;;     :accessor tiles
;;     :initform (make-hash-table :test 'equalp)
;;     :documentation "The hash of the Tile objects which make up the map, stored by xy coordinates.")
;;    (units
;;     :initarg :units
;;     :accessor units
;;     :initform '()
;;     :documentation "A hash of the Unit objects on the map, stored by xy coordinates.")))

;; (defstruct tile
;;   hexagon elevation terrain depth)

;; (defmethod insert-tile ((g grid) (new-tile tile))
;;   "Insert hexagon `h' into the `tile-hash' of grid `g'."
;;   (if (gethash (offset-from-hex (tile-hexagon new-tile)) (tiles g))
;;       (gethash (offset-from-hex (tile-hexagon new-tile)) (tiles g))
;;       (setq (gethash (offset-from-hex (tile-hexagon new-tile)) (tiles g)) new-tile)))

;; Begin by defining a Hexagon class. We are using the Cubic constructor and
;; storage method described by Amin at Red Blob Games.

(defstruct hexagon ()
  q r s)

(defun new-hexagon (&key q r s)
  (if (eq (+ q r s) 0)
      (make-hexagon :q q :r r :s s)))

;; Code to convert from qrs to xy and back again.
(defun hex-from-offset (&key col row)
  "Creates hexes from the .board files used by MegaMek, which use an offset coordinate system instead of cubic like I do."
  (let ((q col)
        (r (- row (floor (/ (+ col (* (bit-and col) -1)) 2))))
        (s (+ (* q -1) r)))
    (new-hexagon :q q :r r :s s)))

(defun offset-from-hex (hex)
  "Creates xy coordinates from cubic coordinates using a hexagon."
  (let ((row (+ (hexagon-r hex) (floor (/ (+ (hexagon-q q) (* (bit-and (hexagon-q q)) -1))))))
        (col (hexagon-q q)))
    (list col row)))

(defun same-hex (hex1 hex2)
  "Hexagons are equal if their q, r, and s coordinates are the same."
  (and (= (hexagon-s hex1) (hexagon-s hex2))
       (= (hexagon-r hex1) (hexagon-r hex2))
       (= (hexagon-q hex1) (hexagon-q hex2))))

(defun hex-addition (hex1 hex2)
  "Uses Cartesian addition to add two hexagons together."
  (new-hexagon :q (+ (hexagon-q hex1) (hexagon-q hex2))
                :r (+ (hexagon-r hex1) (hexagon-r hex2))
                :s (+ (hexagon-s hex1) (hexagon-s hex2))))

(defun hex-subtract (hex1 hex2)
  "Uses Cartesian subtraction to subtract hexagon b from hexagon a."
  (new-hexagon :q (- (hexagon-q a) (hexagon-q b))
                :r (- (hexagon-r a) (hexagon-r b))
                :s (- (hexagon-s a) (hexagon-s b))))

(defun hex-multiply (hex x)
  "Uses Cartesian multiplication to multiply a hex by a value x together."
  (new-hexagon :q (* (hexagon-q hex) x)
                :r (* (hexagon-r hex) x)
                :s (* (hexagon-s hex) x)))

(defun hex-distance (hex1 hex2)
  "The length of the distance between two hexagons is calculated similarly to
Manhattan distances with a square grid, but you half the sum to get the final
distance."
  (let ((hex-length (hex-subtract hex1 hex2)))
    (/ (+ (abs (hexagon-q hex)) (abs (hexagon-r hex)) (abs (hexagon-s hex))) 2)))

(defvar *hex-directions* (vector (new-hexagon :q 1 :r 0 :s -1)
                                 (new-hexagon :q 1 :r -1 :s 0)
                                 (new-hexagon :q 0 :r -1 :s 1)
                                 (new-hexagon :q -1 :r 0 :s 1)
                                 (new-hexagon :q -1 :r 1 :s 0)
                                 (new-hexagon :q 0 :r 1 :s -1))
  "This vector describes the offsets to calculate neighbors.")

(defun hex-direction (direction)
  "Returns the hex in a given direction."
  (if (and (<= 0 direction) (> 6 direction))
      (elt *hex-directions* direction)))

(defun hex-neighbor (hex direction)
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

(defun hex-to-pixel (hex layout)
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

(defun pixel-to-hex (mouse-point layout)
  "Converts from an x,y pixel address to a q,r,s hex address. Called to determine which
hex is being clicked on / hovered over."
  (let* ((vec (layout-pixel-to-hex-matrix layout))
         (modified-point (make-point (/ (- (point-x mouse-point) (layout-x-origin layout)) (layout-x-size layout))
                                     (/ (- (point-y mouse-point) (layout-y-origin layout)) (layout-y-size layout))))
         (calc-q (+ (* (elt vec 0) (point-x modified-point)) (* (elt vec 1) (point-y modified-point))))
         (calc-r (+ (* (elt vec 2) (point-x modified-point)) (* (elt vec 3) (point-y modified-point)))))
    (make-hexagon :q calc-q :r calc-r :s (+ (* calc-q -1) (* calc-r -1)))))

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

(defun draw-hex (hex layout)
  (let ((center (hex-to-pixel hex layout))
        (points '()))
    (dotimes (i 6)
      (push (find-hex-corner center i layout) points))
     points))

(defun pixel-to-hex (origin-point point-size point)
  "Not yet implemented.")
