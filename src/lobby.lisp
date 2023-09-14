(in-package :megastrike)

(defclass lobby ()
  ((map :initarg :map
        :initform nil
        :accessor lobby/map)
   (forces :initarg :forces
           :initform nil
           :accessor lobby/forces)
   (mul :initarg :mul
        :initform (load-mul (merge-pathnames "data/units/mul.csv" (asdf:system-source-directory :megastrike)))
        :accessor lobby/mul)
   (units :initarg :units
          :initform nil
          :accessor lobby/units)))

(defun check-board ()
  (if (lobby/map *lobby*)
      t
      nil))

(defun check-forces ()
  (and (lobby/forces *lobby*)
       (< 1 (length (string-list/strings (lobby/forces *lobby*))))))

(defun check-units ()
  (let ((forces (loop for f being the hash-values of (lobby/forces *lobby*) collect f)))
    (all-numbers-greater-than-zero (mapcar #'count-units forces))))

(defun all-numbers-greater-than-zero (lst)
  (cond ((null lst) t)
        ((>= 0 (car lst)) nil)
        (t (all-numbers-greater-than-zero (cdr lst)))))

(defun draw-lobby-screen ()
  (setf *lobby* (make-instance 'lobby))
  (setf (lobby/forces *lobby*) (create-string-list (make-hash-table :test #'equal))
        (lobby/units *lobby*) (create-string-list (make-hash-table :test #'equal)))
  (let ((layout (gtk:make-grid)))
    (let ((map-selection (draw-map-selection))
          (force-setup (draw-force-setup))
          (unit-selection (draw-mul-list))
          (unit-list (draw-unit-list)))
    (setf (gtk:grid-column-homogeneous-p layout) nil)
    (setf (gtk:widget-hexpand-p map-selection) t
          (gtk:widget-vexpand-p map-selection) t
          (gtk:widget-hexpand-p force-setup) t
          (gtk:widget-vexpand-p force-setup) t
          (gtk:widget-hexpand-p unit-selection) t
          (gtk:widget-vexpand-p unit-selection) t
          (gtk:widget-hexpand-p unit-list) t
          (gtk:widget-vexpand-p unit-list) t)
    (gtk:grid-attach layout map-selection  0 0 1 1)
    (gtk:grid-attach layout unit-selection 1 0 2 1)
    (gtk:grid-attach layout force-setup    0 1 1 1)
    (gtk:grid-attach layout unit-list      1 1 2 1)
    layout)))

;;; Map Section

(defun draw-map-selection ()
  (let ((layout (gtk:make-grid))
        (header      (gtk:make-label :str "<big>Map Selection</big>"))
        (width-label (gtk:make-label :str "<b>Map Width: </b>"))
        (width-entry (gtk:make-entry))
        (height-label (gtk:make-label :str "<b>Map Height: </b>"))
        (height-entry (gtk:make-entry))
        (map-created (gtk:make-label :str "No Map Created."))
        (create-button (gtk:make-button :label "Create Map")))
    (when (check-board)
      (setf (gtk:label-label map-created) "Map Created."))
    (gtk:connect create-button "clicked"
                 (lambda (button)
                   (declare (ignore widget))
                   (let ((w (parse-integer (gtk:entry-buffer-text (gtk:entry-buffer width-entry)) :junk-allowed t))
                         (h (parse-integer (gtk:entry-buffer-text (gtk:entry-buffer height-entry)) :junk-allowed t)))
                     (when (and w h)
                       (setf (lobby/map *lobby*) (make-board w h))
                       (setf (gtk:label-label map-created) "Map Created.")))))
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

;;; Force Section

(defun draw-force-setup ()
  (let (name deploy)
    (let* ((layout (gtk:make-grid))
           (name-label (gtk:make-label :str "Unit Selection"))
           (name-entry (gtk:make-entry))
           (color-selection-dialog (gtk:make-color-dialog))
           (color-selection (gtk:make-color-dialog-button :dialog color-selection-dialog))
           (deploy-label (gtk:make-label :str "Deployment Zone"))
           (deploy-entry (gtk:make-entry))
           (new-force-btn (gtk:make-button :label "New Force")))
      (setf (gtk:grid-column-homogeneous-p layout) nil)
      (string-list/add-label-column (lobby/forces *lobby*) "Force" #'force/name "string" #'force/name)
      (string-list/add-color-column (lobby/forces *lobby*) "Color" #'force/color)
      (string-list/add-label-column (lobby/forces *lobby*) "Deploy Zone" #'force/deployment "string" #'force/deployment)
      (string-list/add-label-column (lobby/forces *lobby*) "PV" #'force-pv "int" #'force-pv)
      (gtk:connect deploy-entry "changed"
                    (lambda (entry)
                      (setf deploy (ignore-errors
                                  (gtk:entry-buffer-text (gtk:entry-buffer deploy-entry))))))
      (gtk:connect name-entry "changed"
                    (lambda (entry)
                      (setf name (ignore-errors
                                  (gtk:entry-buffer-text (gtk:entry-buffer name-entry))))))
      (gtk:connect new-force-btn "clicked"
                    (lambda (button)
                      (declare (ignore button))
                      (let ((color (gdk:rgba-to-string
                                    (gtk:color-dialog-button-rgba color-selection))))
                        (if (and name deploy color)
                            (let ((f (new-force name color deploy)))
                              (string-list/add-item (lobby/forces *lobby*) f name))))))

      (gtk:grid-attach layout name-label                                0 0 1 1)
      (gtk:grid-attach layout name-entry                                1 0 1 1)
      (gtk:grid-attach layout deploy-label                              0 1 1 1)
      (gtk:grid-attach layout deploy-entry                              1 1 1 1)
      (gtk:grid-attach layout color-selection                           0 2 1 1)
      (gtk:grid-attach layout new-force-btn                             1 2 1 1)
      (gtk:grid-attach layout (string-list/view (lobby/forces *lobby*)) 0 3 2 1)
      layout)))

;;; MUL Section

(defun draw-mul-list ()
  (let* ((layout (gtk:make-grid))
         (scroll (gtk:make-scrolled-window)))
    (setf (lobby/mul *lobby*) (create-string-list *mul* :filter-object (make-instance 'mek :type +GROUND-UNITS+) :filter-func #'filter-mek))
    (string-list/add-label-column (lobby/mul *lobby*) "Unit" #'mek/full-name "string" #'mek/full-name)
    (string-list/add-label-column (lobby/mul *lobby*) "Role" #'mek/role "string" #'mek/role)
    (string-list/add-label-column (lobby/mul *lobby*) "Type" #'mek/type "string" #'mek/type)
    (string-list/add-label-column (lobby/mul *lobby*) "PV" #'mek/pv "int" #'mek/pv)
    (string-list/add-label-column (lobby/mul *lobby*) "Size" #'mek/size "int" #'mek/size)
    (string-list/add-label-column (lobby/mul *lobby*) "MV" #'print-movement "string" #'mek/movement)
    (string-list/add-label-column (lobby/mul *lobby*) "TMM" #'mek/tmm "int" #'mek/tmm)
    (string-list/add-label-column (lobby/mul *lobby*) "Armor" #'mek/armor "int" #'mek/armor)
    (string-list/add-label-column (lobby/mul *lobby*) "Structure" #'mek/structure "int" #'mek/structure)
    ;; (string-list/add-label-column (lobby/mul *lobby*) "Threshold" #'mek/threshold "int" #'mek/threshold)
    (string-list/add-label-column (lobby/mul *lobby*) "Short" #'mek/short-str "int" #'mek/comparable-short)
    (string-list/add-label-column (lobby/mul *lobby*) "Medium" #'mek/medium-str "int" #'mek/comparable-medium)
    (string-list/add-label-column (lobby/mul *lobby*) "Long" #'mek/long-str "int" #'mek/comparable-long)
    (string-list/add-label-column (lobby/mul *lobby*) "Extreme" #'mek/extreme-str "int" #'mek/comparable-extreme)
    (string-list/add-label-column (lobby/mul *lobby*) "OV" #'mek/ov "int" #'mek/ov)
    (string-list/add-label-column (lobby/mul *lobby*) "Abilities" #'mek/abilities "string" #'mek/abilities)

    (loop :for label in (list "Ground Units" "Battlemechs" "All Mechs" "Conventional Units"
                          "Vehicles" "Infantry")
          :for filt-type in (list +ground-units+ +bm-units+ +mech-units+ +conventional-units+
                                  +vehicle-units+ +infantry-units+)
          :for col in (list 0 1 2 3 4 5)
          :do
          (let ((btn (gtk:make-button :label label))
                (filt (string-list/filter-object (lobby/mul *lobby*)))
                (filt-type filt-type))
            (gtk:connect btn "clicked"
                         (lambda (button)
                           (declare (ignore button))
                           (setf (mek/type filt) filt-type)
                           (gtk:filter-changed (string-list/filter (lobby/mul *lobby*)) gtk:+filter-change-different+)))
            (gtk:grid-attach layout btn col 0 1 1)))

    (let ((name-label (gtk:make-label :str "Chassis:"))
          (name-entry (gtk:make-entry))
          (search-btn (gtk:make-button :label "Search")))
      (gtk:connect name-entry "changed"
                   (lambda (entry)
                     (setf (mek/chassis (string-list/filter-object (lobby/mul *lobby*)))
                           (ignore-errors
                            (gtk:entry-buffer-text (gtk:entry-buffer name-entry))))))
      (gtk:connect search-btn "clicked"
                   (lambda (button)
                     (declare (ignore button))
                     (gtk:filter-changed (string-list/filter (lobby/mul *lobby*)) gtk:+filter-change-different+)))
      (gtk:grid-attach layout name-label 0 1 1 1)
      (gtk:grid-attach layout name-entry 1 1 1 1)
      (gtk:grid-attach layout search-btn 2 1 1 1))

    (setf (gtk:widget-hexpand-p scroll) t
          (gtk:widget-vexpand-p scroll) t)
    (setf (gtk:scrolled-window-child scroll) (string-list/view (lobby/mul *lobby*)))
    (gtk:grid-attach layout scroll 0 2 6 1)

    (let ((pname "")
          (pskill "")
          (pname-label (gtk:make-label :str "Pilot Name:"))
          (pname-entry (gtk:make-entry))
          (pskill-label (gtk:make-label :str "Pilot Skill:"))
          (pskill-entry (gtk:make-entry))
          (new-unit-btn (gtk:make-button :label "Add Unit")))

      (gtk:connect pname-entry "changed"
                   (lambda (entry)
                     (setf pname (ignore-errors
                                  (gtk:entry-buffer-text (gtk:entry-buffer pname-entry))))))

      (gtk:connect pskill-entry "changed"
                   (lambda (entry)
                     (setf pskill (ignore-errors
                                   (gtk:entry-buffer-text (gtk:entry-buffer pskill-entry))))))

      (gtk:connect new-unit-btn "clicked"
                   (lambda (button)
                     (declare (ignore button))
                     (setf pname (ignore-errors
                                  (gtk:entry-buffer-text (gtk:entry-buffer pname-entry))))
                     (setf pskill (ignore-errors
                                   (gtk:entry-buffer-text (gtk:entry-buffer pskill-entry))))
                     (let ((skill (parse-integer pskill :junk-allowed t))
                           (selected (string-list/selected (lobby/mul *lobby*))))
                       (when (and pname skill selected)
                         (let ((cu (new-combat-unit
                                    :mek selected
                                    :force (string-list/selected (lobby/forces *lobby*))
                                    :pv-mod (calculate-pv-modifier (mek/pv selected) skill)
                                    :pilot (make-pilot :name pname :skill skill))))
                           (string-list/add-item (lobby/units *lobby*) cu (cu/full-name cu))
                           (string-list/brute-update (lobby/forces *lobby*)))))))

      (gtk:grid-attach layout pname-label  0 3 1 1)
      (gtk:grid-attach layout pname-entry  1 3 1 1)
      (gtk:grid-attach layout pskill-label 2 3 1 1)
      (gtk:grid-attach layout pskill-entry 3 3 1 1)
      (gtk:grid-attach layout new-unit-btn 4 3 1 1))

    layout))

;;; Combat Unit Section

(defun draw-unit-list ()
  (let* ((layout (gtk:make-grid))
         (scroll (gtk:make-scrolled-window)))
    (string-list/add-label-column (lobby/units *lobby*) "Unit Name" #'cu/full-name "string" #'cu/full-name)
    (string-list/add-label-column (lobby/units *lobby*) "PV" #'cu/pv "int" #'cu/pv)
    (string-list/add-label-column (lobby/units *lobby*) "Pilot" #'print-pilot "string" #'cu/pilot)
    (string-list/add-label-column (lobby/units *lobby*) "Force" #'print-force "string" #'cu/force)
    (string-list/add-label-column (lobby/units *lobby*) "Size" #'cu/size "int" #'cu/size)
    (string-list/add-label-column (lobby/units *lobby*) "MV" #'print-movement "string" #'cu/movement)
    (string-list/add-label-column (lobby/units *lobby*) "A/S" #'cu/arm-struct "string" #'cu/arm-struct)
    (string-list/add-label-column (lobby/units *lobby*) "Attack" #'cu/attack-string "string" #'cu/attack-string)
    (setf (gtk:widget-hexpand-p scroll) t
          (gtk:widget-vexpand-p scroll) t)
    (setf (gtk:scrolled-window-child scroll) (string-list/view (lobby/units *lobby*)))
    (gtk:grid-attach layout scroll 0 0 1 1)

    layout))
