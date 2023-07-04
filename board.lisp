(in-package #:alphastrike)

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

(defmethod insert-tile ((g grid) (ti tile))
  "Insert tile `ti' into the `tile-hash' of grid `g'."
  (if (gethash (offset-from-hex (tile-hexagon ti)) (tiles g))
      (gethash (offset-from-hex (tile-hexagon ti)) (tiles g))
      (setf (gethash (offset-from-hex (tile-hexagon ti)) (tiles g)) ti)))

(defun new-tile (line)
  (let* ((tile-list (parse-hex-line line))
         (hex-addr (parse-hex-address (nth 0 tile-list))))
    (make-tile :hexagon (hex-from-offset :col (first hex-addr) :row (second hex-addr))
               :elevation (parse-integer (nth 1 tile-list))
               :terrain (nth 2 tile-list)
               :depth (nth 3 tile-list))))

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

(defun load-board-file (f grid)
  (let ((file-lines (uiop:read-file-lines f)))
    (dolist (l file-lines)
      (if (search "hex" l)
           (insert-tile grid (new-tile l))))))
