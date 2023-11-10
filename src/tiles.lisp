(in-package :megastrike)

;;; Tile class

(defclass tile (hexagon)
  ((elevation :initarg :elevation :accessor tile/elevation
    :documentation "The elevation of the tile.")
   (terrain :initarg :terrain :accessor tile/terrain
    :documentation "A plist representing the terrain type and terrain height in a cell.")
   (terrain-palette :initarg :terrain-palette :accessor tile/terrain-palette
    :documentation "The color palette and design to color the tile.")))

(defun new-tile-from-board (line)
  (let* ((tile-list (parse-hex-line line))
         (hex-addr (parse-hex-address (nth 0 tile-list)))
         (terrain-list (parse-terrain-string (nth 2 tile-list))))
    (new-tile (first hex-addr) (second hex-addr)
              (parse-integer (nth 1 tile-list))
              terrain-list
              (nth 3 tile-list))))

(defun empty-string-p (str)
  (string= "" str))

(defun parse-terrain-string (str)
  (remove-if #'empty-string-p (ppcre:all-matches-as-strings (ppcre:create-scanner ".*?(?=;|$)") *test-terrain-str*)))

(defun new-tile (x y &optional (elevation 0) (terrain "clear") (palette "grasslands"))
  (let ((temp-hex (hex-from-offset :col x :row y)))
    (make-instance 'tile
                 :q (hexagon-q temp-hex)
                 :r (hexagon-r temp-hex)
                 :s (hexagon-s temp-hex)
                 :elevation elevation
                 :terrain terrain
                 :terrain-palette palette)))
