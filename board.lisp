(in-package #:alphastrike)

;;; Tile class

(defclass tile ()
  ((hexagon
    :initarg :hexagon
    :accessor tile-hexagon
    :documentation "The hexagon where this tile is located.")
   (elevation
    :initarg :elevation
    :accessor tile-elevation
    :documentation "The elevation of the tile.")
   (terrain
    :initarg :terrain
    :accessor tile-terrain
    :documentation "The terrain string from the .board file for this tile.")
   (terrain-palette
    :initarg :terrain-palette
    :accessor tile-terrain-palette
    :documentation "The color palette and design to color the tile.")))

(defun new-tile (line)
  (let* ((tile-list (parse-hex-line line))
         (hex-addr (parse-hex-address (nth 0 tile-list))))
    (make-instance 'tile
                   :hexagon (hex-from-offset :col (first hex-addr) :row (second hex-addr))
                   :elevation (parse-integer (nth 1 tile-list))
                   :terrain (nth 2 tile-list)
                   :terrain-palette (nth 3 tile-list))))

(define-presentation-method present (tile
                                     (type tile)
                                     stream
                                     (view textual-view) &key)
  (draw-polygon stream (draw-hex (tile-hexagon tile) *layout*) :filled t :ink +light-green+)
  (draw-polygon stream (draw-hex (tile-hexagon tile) *layout*) :filled nil :line-thickness 2)
  (let ((x (first (offset-from-hex (tile-hexagon tile))))
        (y (second (offset-from-hex (tile-hexagon tile)))))
    (draw-text stream (format nil "~2,'0D~2,'0D" x y) (nth 3 (draw-hex (tile-hexagon tile) *layout*)))))

;;; Grid class

(defclass grid ()
  ((tile-hash
    :initarg :tiles
    :accessor tiles
    :initform (make-hash-table :test 'equalp)
    :documentation "The hash of the Tile objects which make up the map, stored by xy coordinates.")
   (units
    :initarg :units
    :accessor units
    :initform (make-hash-table :test 'equalp)
    :documentation "A hash of the Unit objects on the map, stored by xy coordinates.")))

(defmethod add-unit ((g grid) (u combat-unit))
  (if (not (gethash (cu-id u) (units g)))
      (setf (gethash (cu-id u) (units g)) u)))

(defmethod insert-tile ((g grid) (ti tile))
  "Insert tile `ti' into the `tile-hash' of grid `g'."
  (if (gethash (offset-from-hex (tile-hexagon ti)) (tiles g))
      (gethash (offset-from-hex (tile-hexagon ti)) (tiles g))
      (setf (gethash (offset-from-hex (tile-hexagon ti)) (tiles g)) ti)))

;;; Board file parsing logic

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
