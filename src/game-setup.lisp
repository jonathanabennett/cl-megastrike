(in-package :megastrike)

(defvar *master-unit-list* '())

(defun load-data ()
  "Load the contents of the data directory in prepration for execution."
  (let ((mech-files (uiop:directory-files (uiop:merge-pathnames* #p"data/units/" *here*))))
    (dolist (file mech-files)
      (if (string= (pathname-type file) "lisp")
          (load file)))))

(defun build-mul ()
  (setf *master-unit-list* (load-database)))

(defun make-combat-unit (unit-fn offset-hex-addr pilot skill force
                         &optional (struct nil) (armor nil) (crits '()) (heat 0))
  (let ((u (funcall unit-fn))
        (hex (hex-from-offset :col (first offset-hex-addr) :row (second offset-hex-addr))))
    (setf (location/q u) (hexagon-q hex))
    (setf (location/r u) (hexagon-r hex))
    (setf (location/s u) (hexagon-s hex))
    (if struct (setf (damageable/cur-struct u) struct))
    (if armor  (setf (damageable/cur-armor  u) armor))
    (setf (damageable/crit-list u) crits)
    (setf (heat/cur-heat u) heat)
    (setf (pilot/name u) pilot)
    (setf (pilot/skill u) skill)
    (add-unit force u)))
