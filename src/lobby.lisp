(in-package :megastrike)

(defclass lobby ()
  ((selected-mek  :initform nil :accessor lobby/selected-mek)
   (game          :initform nil :accessor lobby/game
                  :initarg :game)))
(defun new-lobby ()
  (let ((g (new-game)))
    (make-instance 'lobby :game g)))

(defun draw-lobby-screen ()
  (load-mul (merge-pathnames "data/units/mul.csv" (asdf:system-source-directory :megastrike)))
  (sleep 0.5)
  (let ((layout (gtk:make-grid)))
    (let ((map-selection (gtk:make-label :str "Map position"))
          (force-setup (draw-force-setup))
          (unit-selection (draw-mul-list))
          (unit-list (gtk:make-label :str "Combat Units Position")))
    (setf (gtk:widget-hexpand-p map-selection) t
          (gtk:widget-vexpand-p map-selection) t
          (gtk:widget-hexpand-p force-setup) t
          (gtk:widget-vexpand-p force-setup) t
          (gtk:widget-hexpand-p unit-selection) t
          (gtk:widget-vexpand-p unit-selection) t
          (gtk:widget-hexpand-p unit-list) t
          (gtk:widget-vexpand-p unit-list) t)
    (gtk:grid-attach layout map-selection  0 0 1 1)
    (gtk:grid-attach layout unit-selection 1 0 1 1)
    (gtk:grid-attach layout force-setup    0 1 1 1)
    (gtk:grid-attach layout unit-list      1 1 1 1)
    layout)))

(defun draw-unit-list ()
  (let ((layout (gtk:make-grid))
        (name-label (gtk:make-label :str "Force Name"))
        (name-entry (gtk:make-entry))
        (color-selection (gtk:make-color-button))
        (deploy-label (gtk:make-label :str "Deployment Zone"))
        (deploy-entry (gtk:make-entry)))
    (gtk:grid-attach layout name-label 0 0 1 1)
    (gtk:grid-attach layout name-entry 1 0 1 1)
    (gtk:grid-attach layout color-selection    0 1 1 1)
    (gtk:grid-attach layout deploy-label      1 1 1 1)
    (gtk:grid-attach layout deploy-entry  2 2 1 1)
    layout))

(defun check-board ()
  (if (game/board *game*)
      t
      nil))

(defun check-forces ()
  (and (game/forces *game*)
       (< 1 (length (game/forces *game*)))))

(defun check-units ()
  (let ((unit-counts (mapcar #'count-units (game/forces *game*))))
    (all-numbers-greater-than-zero unit-counts)))

(defun all-numbers-greater-than-zero (lst)
  (cond ((null lst) t)
        ((>= 0 (car lst)) nil)
        (t (all-numbers-greater-than-zero (cdr lst)))))

(defun draw-map-selection ()
  (let ((layout (gtk:make-grid))
        (header      (gtk:make-label :str "<big>Map Selection</big>"))
        (width-label (gtk:make-label :str "<b>Map Width: </b>"))
        (width-entry (gtk:make-entry))
        (height-label (gtk:make-label :str "<b>Map Height: </b>"))
        (height-entry (gtk:make-entry))
        (map-created (gtk:make-label :str "No Map Created.")))
    ;;     (create-button (gtk-button-new-with-label "Create Map")))
    ;; (if (check-board)
    ;;     (gtk-label-set-text map-created "Map Created."))
    ;; (g-signal-connect create-button "clicked"
    ;;                   (lambda (widget)
    ;;                     (declare (ignore widget))
    ;;                     (let ((w (parse-integer (gtk-entry-text width-entry)))
    ;;                           (h (parse-integer (gtk-entry-text height-entry))))
    ;;                       (if (and w h)
    ;;                           (setf (game/board *game*) (make-grid w h)))
    ;;                       (gtk-label-set-text map-created "Map Created."))))
    (setf (gtk:label-use-markup-p header) t
          (gtk:label-use-markup-p width-label) t
          (gtk:label-use-markup-p height-label) t)
    (gtk:grid-attach layout header 0 0 2 1)
    (gtk:grid-attach layout width-label 0 1 1 1)
    (gtk:grid-attach layout width-entry 1 1 1 1)
    (gtk:grid-attach layout height-label 2 1 1 1)
    (gtk:grid-attach layout height-entry 3 1 1 1)
    (gtk:grid-attach layout create-button 1 2 1 1)
    (gtk:grid-attach layout map-created 0 2 1 1)
    layout))
