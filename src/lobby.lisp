(in-package :megastrike)

(defclass lobby ()
  ((toplevel :accessor lobby/layout
             :initform (gtk:make-grid)
             :documentation "The widget containing the lobby screen.")
   (map-setup-grid :accessor lobby/map-layout
                    :initform (gtk:make-grid)
                    :documentation "The widget containing map layout information.")
   (forces-setup-grid :accessor lobby/forces-layout
                      :initform (gtk:make-grid)
                      :documentation "The widget containing forces layout information.")
   (mul-setup-grid :accessor lobby/mul-layout
                   :initform (gtk:make-grid)
                   :documentation "The widget containing mul layout information.")
   (unit-list-box :accessor lobby/unit-list
                   :initarg unit-list
                   :initform (gtk:make-list-box)
                   :documentation "The widget containing the list of units in the game.")
   (map :accessor lobby/map
        :initarg :map
        :initform nil
        :documentation "The map for the game being prepared.")
   (forces :accessor lobby/forces
           :initarg :forces
           :initform nil
           :documentation "The forces for the game being prepared")
   (mul :accessor lobby/mul
        :initarg :mul
        :initform (load-mul (merge-pathnames "units/mul.csv" *data-folder*))
        :documentation "The list of all possible meks that could be piloted in the game.")
   )
  (:documentation "The lobby used to build the game before it launches."))

(defun check-board ()
  "Return `t' if a board has been created."
  (if (lobby/map *lobby*)
      t
      nil))

(defun check-forces ()
  "Return `t' if there are at least 2 forces in the game."
  (and (lobby/forces *lobby*)
       (< 1 (length (string-list/strings (lobby/forces *lobby*))))))

(defun check-units ()
  "Return `t' if each force has at least 1 unit."
  (let ((forces (loop for f being the hash-values of (string-list/source (lobby/forces *lobby*))
                      collect f)))
    (all-numbers-greater-than-zero (mapcar #'count-units forces))))

(defun all-numbers-greater-than-zero (lst)
  "Helper function for `check-units'. Return `t' if every number in the list `lst' is > 0."
  (cond ((null lst) t)
        ((>= 0 (car lst)) nil)
        (t (all-numbers-greater-than-zero (cdr lst)))))

(defun game-ready-p ()
  "Return `t' if the game is ready to play."
  (and (check-board)
       (check-forces)
       (check-units)))

(defun draw-lobby-screen ()
  "GTK Function to draw the lobby."
  (let ((window (gtk:make-application-window :application gtk:*application*))
        (layout (lobby/layout *lobby*))
        (button-bar (gtk:make-box :orientation gtk:+orientation-horizontal+ :spacing 5)))
    (let ((button (gtk:make-button :label "Not Ready")))
          (setf (gtk:widget-sensitive-p button) nil)
          (gtk:connect button "clicked" (lambda (button)
                                          (declare (ignore button))
                                          (start-game)
                                          (gtk:window-destroy window)))
          (gtk:timeout-add 500 (lambda ()
                                 (if (game-ready-p)
                                     (progn
                                       (setf (gtk:button-label button) "Game Ready")
                                       (setf (gtk:widget-sensitive-p button) t)
                                       glib:+source-remove+)
                                     glib:+source-continue+)))
          (gtk:box-append button-bar button))
    (let ((button (gtk:make-button :label "Exit Game")))
      (gtk:connect button "clicked" (lambda (button)
                                      (declare (ignore button))
                                      (gtk:window-destroy window)))
      (gtk:box-append button-bar button))
    (draw-map-selection)
    (draw-force-setup)
    (draw-mul-list)
    (draw-unit-list)
    (let ((unit-scroll (gtk:make-scrolled-window)))

      (setf (gtk:grid-column-homogeneous-p layout) nil
            (gtk:grid-row-homogeneous-p layout) nil
            (gtk:scrolled-window-child unit-scroll) (lobby/unit-list *lobby*))
      (gtk:grid-attach layout (lobby/mul-layout *lobby*)   0 0 2 1)
      (gtk:grid-attach layout (lobby/forces-layout *lobby*) 2 0 1 1)
      (gtk:grid-attach layout unit-scroll                  0 1 2 1)
      (gtk:grid-attach layout (lobby/map-layout *lobby*)   2 1 1 1)
      (gtk:grid-attach layout button-bar                   0 2 3 1)
      (setf (gtk:window-child window) layout)
      (unless (gtk:widget-visible-p window)
        (gtk:window-present window)))))

;;; Map Section

(defun draw-map-selection ()
  "GTK Function drawing the map selection section on the screen."
  (let ((layout (lobby/map-layout *lobby*))
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
    (gtk:grid-attach layout height-label 0 2 1 1)
    (gtk:grid-attach layout height-entry 1 2 1 1)
    (gtk:grid-attach layout map-created 0 3 1 1)
    (gtk:grid-attach layout create-button 1 3 1 1)))

;;; Force Section

(defun draw-force-setup ()
  (let (name deploy)
    (let* ((layout (lobby/forces-layout *lobby*))
           (name-label (gtk:make-label :str "Force Name: "))
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
                        (when (and name deploy color)
                          (let ((f (new-force name color deploy)))
                            (string-list/add-item (lobby/forces *lobby*) f name))))))
      (gtk:grid-attach layout name-label                                0 0 1 1)
      (gtk:grid-attach layout name-entry                                1 0 1 1)
      (gtk:grid-attach layout deploy-label                              0 1 1 1)
      (gtk:grid-attach layout deploy-entry                              1 1 1 1)
      (gtk:grid-attach layout color-selection                           0 2 1 1)
      (gtk:grid-attach layout new-force-btn                             1 2 1 1)
      (gtk:grid-attach layout (string-list/view (lobby/forces *lobby*)) 0 3 2 1))))

;;; MUL Section

(defun draw-mul-list ()
  (let* ((layout (lobby/mul-layout *lobby*))
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

    (loop :for label in (list "Ground Units" "Battlemechs" "All Mechs" "Conventional Units" "Vehicles" "Infantry")
          :for filt-type in (list +ground-units+ +bm-units+ +mech-units+ +conventional-units+ +vehicle-units+ +infantry-units+)
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
                     (declare (ignore entry))
                     (setf (mek/chassis (string-list/filter-object (lobby/mul *lobby*)))
                           (ignore-errors (gtk:entry-buffer-text (gtk:entry-buffer name-entry))))))
      (gtk:connect search-btn "clicked"
                   (lambda (button)
                     (declare (ignore button))
                     (gtk:filter-changed (string-list/filter (lobby/mul *lobby*)) gtk:+filter-change-different+)))
      (gtk:grid-attach layout name-label 0 1 1 1)
      (gtk:grid-attach layout name-entry 1 1 1 1)
      (gtk:grid-attach layout search-btn 2 1 1 1))

    (setf (gtk:widget-hexpand-p scroll) t
          (gtk:widget-vexpand-p scroll) t
          (gtk:scrolled-window-child scroll) (string-list/view (lobby/mul *lobby*)))
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
                     (declare (ignore entry))
                     (setf pname (ignore-errors (gtk:entry-buffer-text (gtk:entry-buffer pname-entry))))))

      (gtk:connect pskill-entry "changed"
                   (lambda (entry)
                     (declare (ignore entry))
                     (setf pskill (ignore-errors (gtk:entry-buffer-text (gtk:entry-buffer pskill-entry))))))

      (gtk:connect new-unit-btn "clicked"
                   (lambda (button)
                     (declare (ignore button))
                     (setf pname (ignore-errors (gtk:entry-buffer-text (gtk:entry-buffer pname-entry))))
                     (setf pskill (ignore-errors (gtk:entry-buffer-text (gtk:entry-buffer pskill-entry))))
                     (let ((skill (parse-integer pskill :junk-allowed t))
                           (selected (string-list/selected (lobby/mul *lobby*))))
                       (when (and pname skill selected)
                         (let ((cu (new-combat-unit
                                    :mek selected
                                    :force (string-list/selected (lobby/forces *lobby*))
                                    :pv-mod (calculate-pv-modifier (mek/pv selected) skill)
                                    :pilot (make-pilot :name pname :skill skill))))
                           (push cu (game/units *game*))
                           (gtk:list-box-append (lobby/unit-list *lobby*) (draw-quickstats cu))
                           (string-list/brute-update (lobby/forces *lobby*)))))))
      (gtk:grid-attach layout pname-label  0 3 1 1)
      (gtk:grid-attach layout pname-entry  1 3 1 1)
      (gtk:grid-attach layout pskill-label 2 3 1 1)
      (gtk:grid-attach layout pskill-entry 3 3 1 1)
      (gtk:grid-attach layout new-unit-btn 4 3 1 1))))

    ;;; Combat Unit Section

(defun draw-unit-list ()
  (let* ((unit-list (lobby/unit-list *lobby*)))
    (mapcar #'(lambda (unit) (gtk:list-box-append unit-list (draw-quickstats unit))) (game/units *game*))
    (setf (gtk:widget-hexpand-p unit-list) t
          (gtk:widget-vexpand-p unit-list) t)))
