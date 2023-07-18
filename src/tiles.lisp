(in-package :megastrike)

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

(defun draw-tile (tile stream)
  (draw-polygon stream (draw-hex (tile-hexagon tile)
                                 (frame/layout *application-frame*))
                :filled t :ink +light-green+)
  (draw-polygon stream (draw-hex (tile-hexagon tile)
                                 (frame/layout *application-frame*))
                :filled nil :line-thickness 2)
  (let ((x (first (offset-from-hex (tile-hexagon tile))))
        (y (second (offset-from-hex (tile-hexagon tile)))))
    (draw-text stream (format nil "~2,'0D~2,'0D" x y)
               (nth 3 (draw-hex (tile-hexagon tile) (frame/layout *application-frame*))))))


(define-presentation-method present (tile
                                     (type tile)
                                     stream
                                     (view graphical-view) &key)
  (draw-tile tile stream))

(define-presentation-method present (tile
                                     (type tile)
                                     stream
                                     (view textual-view) &key)
  (format stream "~a" (offset-from-hex (tile-hexagon tile))))
;;; Grid class
