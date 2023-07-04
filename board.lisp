(in-package #:alphastrike)

(defun parse-hex-address (str)
"A hex address from a board file is a string in the format xxyy"
  (let ((x (parse-integer (subseq str 0 2)))
        (y (parse-integer (subseq str 2 4))))
    (list x y)))

;;; A line from a .board file looks like this:
;;; hex xxyy elevation terrain style
;;;
;;; For example:
;;; hex 0101 3 "woods:1;foliage_elev:2" "grass"

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
    :initform (make-hash-table :test 'equalp)
    :documentation "The hash of the Tile objects which make up the map, stored by xy coordinates.")
   (units
    :initarg :units
    :accessor units
    :initform '()
    :documentation "A hash of the Unit objects on the map, stored by xy coordinates.")))

(defstruct tile
  hexagon elevation terrain depth)

(defmethod insert-tile ((g grid) (new-tile tile))
  "Insert hexagon `h' into the `tile-hash' of grid `g'."
  (if (gethash (offset-from-hex (tile-hexagon new-tile)) (tiles g))
      (gethash (offset-from-hex (tile-hexagon new-tile)) (tiles g))
      (setq (gethash (offset-from-hex (tile-hexagon new-tile)) (tiles g)) new-tile)))

