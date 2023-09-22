(in-package :megastrike)

;;; Tile class

(defclass tile (hexagon)
  ((occupied-p :initform nil :accessor tile-occupied-p)
   (elevation :initarg :elevation :accessor tile-elevation
    :documentation "The elevation of the tile.")
   (terrain :initarg :terrain :accessor tile-terrain
    :documentation "The terrain string from the .board file for this tile.")
   (terrain-palette :initarg :terrain-palette :accessor tile-terrain-palette
    :documentation "The color palette and design to color the tile.")))

(defun new-tile-from-board (line)
  (let* ((tile-list (parse-hex-line line))
         (hex-addr (parse-hex-address (nth 0 tile-list))))
    (new-tile (first hex-addr) (second hex-addr)
              (parse-integer (nth 1 tile-list))
              (nth 2 tile-list)
              (nth 3 tile-list))))

(defun new-tile (x y &optional (elevation 0) (terrain "clear") (palette "grasslands"))
  (let ((temp-hex (hex-from-offset :col x :row y)))
    (make-instance 'tile
                 :q (hexagon-q temp-hex)
                 :r (hexagon-r temp-hex)
                 :s (hexagon-s temp-hex)
                 :elevation elevation
                 :terrain terrain
                 :terrain-palette palette)))
