(in-package #:megastrike)

(defclass board ()
  ((tile-hash
    :initarg :tiles
    :accessor board/tiles
    :initform (make-hash-table :test 'equalp)
    :documentation "The hash of the Tile objects which make up the map,
stored by xy coordinates.")))


(defmethod insert-tile ((g board) (ti tile))
  "Insert tile `ti' into the `tile-hash' of board `g' if it doesn't exist."
  (if (gethash (offset-from-hex ti) (board/tiles g))
      (gethash (offset-from-hex ti) (board/tiles g))
      (setf (gethash (offset-from-hex ti) (board/tiles g)) ti)))

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

(defun load-mapsheet-file (f board)
  (let ((file-lines (uiop:read-file-lines f)))
    (dolist (l file-lines)
      (when (search "hex" l)
        (insert-tile board (new-tile l))))))

(defun make-board (width height)
  (let ((g (make-instance 'board)))
    (dotimes (x width)
      (dotimes (y height)
        (insert-tile g (new-tile (1+ x) (1+ y)))))
    g))


(defun cairo-draw-hex (loc hex cr window)
  (let ((hex-points (draw-hex hex +default-layout+))
        (hex-center (hex-to-pixel hex +default-layout+)))
    (cairo-set-source-rgb cr 0.0 0.0 0.0)
    (cairo-move-to cr (point-x (first hex-points)) (point-y (first hex-points)))
    (dotimes (i 6)
      (cairo-line-to cr (point-x (nth i hex-points)) (point-y (nth i hex-points))))
    (cairo-close-path cr)
    (cairo-stroke-preserve cr)
    (cairo-set-source-rgb cr 0.0 0.6 0.09)
    (cairo-fill cr)
    (cairo-set-source-rgb cr 0.0 0.0 0.0)
    (cairo-move-to cr (point-x (nth 3 hex-points))(point-y (nth 3 hex-points)))
    (cairo-set-font-size cr 15)
    (cairo-text-path cr (format nil "~2,'0D~2,'0D" (first loc) (second loc)))
    (cairo-fill cr)))
