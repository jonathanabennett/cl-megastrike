(in-package #:megastrike)

(defclass board ()
  ((name
    :initarg :name
    :accessor board/name
    :initform "")
   (tile-hash
    :initarg :tiles
    :accessor board/tiles
    :initform (make-hash-table :test 'equalp)
    :documentation "The hash of the Tile objects which make up the map,
stored by xy coordinates.")
   (width :accessor board/width
          :initarg :width)
   (height :accessor board/height
           :initarg :height)))


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

;; TODO Add a way to figure out the width and height
(defun load-mapsheet-file (f board)
  (let ((mapsheet-name (pathname-name f))
        (file-lines (uiop:read-file-lines f)))
    (setf board/name mapsheet-name)
    (dolist (l file-lines)
      (when (search "hex" l)
        (insert-tile board (new-tile l))))))

(defun make-board (width height)
  (let ((g (make-instance 'board)))
    (setf (board/width g) width)
    (setf (board/height g) height)
    (dotimes (x width)
      (dotimes (y height)
        (insert-tile g (new-tile (1+ x) (1+ y)))))
    g))

