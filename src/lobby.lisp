(in-package :megastrike)

(defun draw-lobby-screen ()
  (load-mul (merge-pathnames "data/units/mul.csv" (asdf:system-source-directory :megastrike)))
  (sleep 0.5)
  (let ((layout (gtk:make-grid)))
    (let ((map-selection (draw-map-selection))
          (force-setup (draw-force-setup))
          (unit-selection (draw-mul-list))
          (unit-list (gtk:make-label :str "Combat Units Position")))
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

(defun draw-unit-list ()
  (let ((layout (gtk:make-grid))
        (model (gtk:make-string-list :strings (loop for id being the hash-keys of (game/units *game*) collect (princ-to-string id))))
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
        (map-created (gtk:make-label :str "No Map Created."))
        (create-button (gtk:make-button :label "Create Map")))
    (if (check-board)
        (setf (gtk:label-label map-created) "Map Created."))
    (gtk:connect create-button "clicked"
                 (lambda (button)
                   (declare (ignore widget))
                   (let ((w (parse-integer (gtk:entry-buffer-text (gtk:entry-buffer width-entry)) :junk-allowed t))
                         (h (parse-integer (gtk:entry-buffer-text (gtk:entry-buffer height-entry)) :junk-allowed t)))
                     (if (and w h)
                         (progn
                           (setf (game/board *game*) (make-board w h))
                           (setf (gtk:label-label map-created) "Map Created."))))))
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

;; FORCE CREATION section
(let (model view)
  (defun new-force-string (f)
    (let ((uuid (format nil "~a" (uuid:make-v1-uuid))))
      (setf (gethash uuid (game/forces-hash *game*)) f)
      (gtk:string-list-append model uuid)))

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
        (force-list-view)
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
                               (new-force-string f))))))
        (gtk:grid-attach layout name-label      0 0 1 1)
        (gtk:grid-attach layout name-entry      1 0 1 1)
        (gtk:grid-attach layout deploy-label    0 1 1 1)
        (gtk:grid-attach layout deploy-entry    1 1 1 1)
        (gtk:grid-attach layout color-selection 0 2 1 1)
        (gtk:grid-attach layout new-force-btn   1 2 1 1)
        (gtk:grid-attach layout view            0 3 2 1)
        layout)))

  (defun force-list-view ()
    (setf model (gtk:make-string-list :strings '()))
    (let* ((rows (loop for uuid being the hash-keys of (game/forces-hash *game*)
                      :collect uuid))

          (name-factory (gtk:make-signal-list-item-factory))
          (name-col (gtk:make-column-view-column :title "Force" :factory name-factory))

          (color-factory (gtk:make-signal-list-item-factory))
          (color-col (gtk:make-column-view-column :title "Color" :factory color-factory))

          (deploy-factory (gtk:make-signal-list-item-factory))
          (deploy-col (gtk:make-column-view-column :title "Deploy Zone" :factory deploy-factory))

          (pv-factory (gtk:make-signal-list-item-factory))
          (pv-col (gtk:make-column-view-column :title "PV" :factory pv-factory)))

      (setf view (gtk:make-column-view :model (gtk:make-single-selection :model model)))

      (gtk:column-view-append-column view name-col)
      (gtk:column-view-append-column view color-col)
      (gtk:column-view-append-column view deploy-col)
      (gtk:column-view-append-column view pv-col)

      (gtk:connect (gtk:column-view-model view) "selection-changed"
                   (lambda (model position n-items)
                     (declare (ignore position n-items))
                     (let ((uuid (gtk:string-object-string (gobj:coerce (gtk:single-selection-selected-item model) 'gtk:string-object))))
                       (setf (game/selected-force *game*) (game/find-force *game* uuid)))))
      (setf (gtk:widget-vexpand-p view) t
            (gtk:widget-hexpand-p view) t)

      (flet ((setup-label (factory item)
               (declare (ignore factory))
               (setf (gtk:list-item-child item) (gtk:make-label :str "")))
             (setup-color (factory item)
               (declare (ignore factory))
               (let (cd (gtk:make-color-dialog))
                 (setf (gtk:list-item-child item) (gtk:make-color-dialog-button :dialog cd))))
             (unbind (factory item) (declare (ignore factory item)))
             (teardown (factory item) (declare (ignore factory item))))
        (loop :for factory :in (list name-factory color-factory deploy-factory pv-factory)
              :for type  :in (list "string" "string" "string" "int")
              :for accessor :in (list #'force/name #'force/color #'force/deployment #'force-pv)
              :do (gtk:connect factory "setup" #'setup-label)
                 (if (not (string= type "color"))
                      (gtk:connect factory "setup" #'setup-label)
                      (gtk:connect factory "setup" #'setup-color))
                  (gtk:connect factory "unbind" #'unbind)
                  (gtk:connect factory "teardown" #'teardown)
                  (gtk:connect factory "bind"
                           (let ((accessor accessor))
                             (lambda (factory item)
                               (declare (ignore factory))
                               (let* ((uuid (gtk:string-object-string (gobj:coerce (gtk:list-item-item item) 'gtk:string-object)))
                                      (row (gethash uuid (game/forces-hash *game*)))
                                      (value (format nil "~a" (funcall accessor row))))
                                 (setf (gtk:label-text (gobj:coerce (gtk:list-item-child item) 'gtk:label)) value))))))))))

;; MUL Section

;; Callback function to filter objects
(cffi:defcallback filter-string-object-via-accessor :bool
    ((item :pointer)
     (data :pointer))
  (let ((filter-func (gethash (cffi:pointer-address data) glib::*objects*)))
    (funcall filter-func (gtk:string-object-string (gobj:pointer-object item 'gtk:string-object)))))

;; Callback function to compare strings
(cffi:defcallback compare-string-object-via-accessor :int
    ((a :pointer)
     (b :pointer)
     (data :pointer))
  (let ((accessor (gethash (cffi:pointer-address data) glib::*objects*))) ; Use (glib::get-object (cffi:pointer-address data)) in the latest version of cl-glib.
    (let ((string-a (funcall accessor (gtk:string-object-string (gobj:pointer-object a 'gtk:string-object))))
          (string-b (funcall accessor (gtk:string-object-string (gobj:pointer-object b 'gtk:string-object)))))
      (if (string= string-a string-b)
          0
          (if (string< string-a string-b) +1 -1)))))

;; Callback function to compare numbers
(cffi:defcallback compare-string-number-object-via-accessor :int
    ((a :pointer)
     (b :pointer)
     (data :pointer))
  (let ((accessor (gethash (cffi:pointer-address data) glib::*objects*))) ; Use (glib::get-object (cffi:pointer-address data)) in the latest version of cl-glib.
    (let ((number-a (funcall accessor (gtk:string-object-string (gobj:pointer-object a 'gtk:string-object))))
          (number-b (funcall accessor (gtk:string-object-string (gobj:pointer-object b 'gtk:string-object)))))
      (if (= number-a number-b)
          0
          (if (< number-a number-b) +1 -1)))))

(defun draw-mul-list ()
  (let* ((layout (gtk:make-grid))
         (scroll (gtk:make-scrolled-window))
         (selected nil)
         (model (gtk:make-string-list :strings (loop for uuid being the hash-keys of *mul*
                                                     :collect uuid)))
         (filt (make-instance 'mek
                              :type +GROUND-UNITS+))
         (filter (gtk:make-custom-filter :match-func (cffi:callback filter-string-object-via-accessor)
                                         :user-data (cffi:make-pointer (glib::put-object (alexandria:compose (alexandria:curry #'filter-mek filt) (alexandria:rcurry #'gethash *mul*))))
                                         :user-destroy (cffi:callback glib::free-object-callback)))
         (view (mul-list-view model)))
    (setf (gtk:column-view-model view)
          (gtk:make-single-selection :model (gtk:make-sort-list-model :model (gtk:make-filter-list-model :model model :filter filter) :sorter (gtk:column-view-sorter view))))
    (gtk:connect (gtk:column-view-model view) "selection-changed"
                 (lambda (model position n-items)
                   (declare (ignore position n-items))
                   (let ((uuid (gtk:string-object-string (gobj:coerce (gtk:single-selection-selected-item model) 'gtk:string-object))))
                     (setf selected (gethash uuid *mul*)))))

    (let ((btn (gtk:make-button :label "All Ground Units")))
      (gtk:connect btn "clicked"
                   (lambda (button)
                     (declare (ignore button))
                     (setf (mek/type filt) +GROUND-UNITS+)
                     (gtk:filter-changed filter gtk:+filter-change-different+)))
      (gtk:grid-attach layout btn 0 0 1 1))

    (let ((btn (gtk:make-button :label "Battlemechs")))
      (gtk:connect btn "clicked"
                   (lambda (button)
                     (declare (ignore button))
                     (setf (mek/type filt) +BM-UNITS+)
                     (gtk:filter-changed filter gtk:+filter-change-different+)))
      (gtk:grid-attach layout btn 1 0 1 1))

    (let ((btn (gtk:make-button :label "All Mechs")))
      (gtk:connect btn "clicked"
                   (lambda (button)
                     (declare (ignore button))
                     (setf (mek/type filt) +MECH-UNITS+)
                     (gtk:filter-changed filter gtk:+filter-change-different+)))
      (gtk:grid-attach layout btn 2 0 1 1))

    (let ((btn (gtk:make-button :label "Conventional Units")))
      (gtk:connect btn "clicked"
                   (lambda (button)
                     (declare (ignore button))
                     (setf (mek/type filt) +CONVENTIONAL-UNITS+)
                     (gtk:filter-changed filter gtk:+filter-change-different+)))
      (gtk:grid-attach layout btn 3 0 1 1))

    (let ((btn (gtk:make-button :label "Vehicles")))
      (gtk:connect btn "clicked"
                   (lambda (button)
                     (declare (ignore button))
                     (setf (mek/type filt) +VEHICLE-UNITS+)
                     (gtk:filter-changed filter gtk:+filter-change-different+)))
      (gtk:grid-attach layout btn 4 0 1 1))

    (let ((btn (gtk:make-button :label "Infantry")))
      (gtk:connect btn "clicked"
                   (lambda (button)
                     (declare (ignore button))
                     (setf (mek/type filt) +INFANTRY-UNITS+)
                     (gtk:filter-changed filter gtk:+filter-change-different+)))
      (gtk:grid-attach layout btn 5 0 1 1))

    (let ((name-label (gtk:make-label :str "Chassis:"))
          (name-entry (gtk:make-entry))
          (search-btn (gtk:make-button :label "Search")))
      (gtk:connect name-entry "changed"
                   (lambda (entry)
                     (setf (mek/chassis filt)
                           (ignore-errors
                            (gtk:entry-buffer-text (gtk:entry-buffer name-entry))))))
      (gtk:connect search-btn "clicked"
                   (lambda (button)
                     (declare (ignore button))
                     (gtk:filter-changed filter gtk:+filter-change-different+)))
      (gtk:grid-attach layout name-label 0 1 1 1)
      (gtk:grid-attach layout name-entry 1 1 1 1)
      (gtk:grid-attach layout search-btn 2 1 1 1))

    (setf (gtk:widget-hexpand-p scroll) t
          (gtk:widget-vexpand-p scroll) t)
    (setf (gtk:scrolled-window-child scroll) view)
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
                     (let ((skill (parse-integer pskill :junk-allowed t)))
                       (format t "~a ~a ~a" pname pskill (mek/full-name selected))
                       (if (and pname skill selected)
                           (new-combat-unit :mek selected :force (game/selected-force *game*)
                                            :pv-mod (calculate-pv-modifier (mek/pv selected) pskill)
                                            :pilot (make-pilot :name pname :skill pskill))))))

      (gtk:grid-attach layout pname-label  0 3 1 1)
      (gtk:grid-attach layout pname-entry  1 3 1 1)
      (gtk:grid-attach layout pskill-label 2 3 1 1)
      (gtk:grid-attach layout pskill-entry 3 3 1 1)
      (gtk:grid-attach layout new-unit-btn 4 3 1 1))

    layout))

(defun mul-list-view (model)
  (let* ((view (gtk:make-column-view :model nil))
         (chassis-factory (gtk:make-signal-list-item-factory))
         (chassis-col (gtk:make-column-view-column :title "Chassis" :factory chassis-factory))
         (role-factory (gtk:make-signal-list-item-factory))
         (role-col (gtk:make-column-view-column :title "Role" :factory role-factory))
         (type-factory (gtk:make-signal-list-item-factory))
         (type-col (gtk:make-column-view-column :title "Type" :factory type-factory))
         (pv-factory (gtk:make-signal-list-item-factory))
         (pv-col (gtk:make-column-view-column :title "PV" :factory pv-factory))
         (size-factory (gtk:make-signal-list-item-factory))
         (size-col (gtk:make-column-view-column :title "Size" :factory size-factory))
         (movement-factory (gtk:make-signal-list-item-factory))
         (movement-col (gtk:make-column-view-column :title "MV" :factory movement-factory))
         (tmm-factory (gtk:make-signal-list-item-factory))
         (tmm-col (gtk:make-column-view-column :title "TMM" :factory tmm-factory))
         (armor-factory (gtk:make-signal-list-item-factory))
         (armor-col (gtk:make-column-view-column :title "A" :factory armor-factory))
         (struct-factory (gtk:make-signal-list-item-factory))
         (struct-col (gtk:make-column-view-column :title "S" :factory struct-factory))
         ;; Commented out until I need them (These are only found in flying units)
         ;; (threshold-factory (gtk:make-signal-list-item-factory))
         ;; (threshold-col (gtk:make-column-view-column :title "Threshold" :factory threshold-factory))
         (short-factory (gtk:make-signal-list-item-factory))
         (short-col (gtk:make-column-view-column :title "S" :factory short-factory))
         (medium-factory (gtk:make-signal-list-item-factory))
         (medium-col (gtk:make-column-view-column :title "M" :factory medium-factory))
         (long-factory (gtk:make-signal-list-item-factory))
         (long-col (gtk:make-column-view-column :title "L" :factory long-factory))
         (extreme-factory (gtk:make-signal-list-item-factory))
         (extreme-col (gtk:make-column-view-column :title "E" :factory extreme-factory))
         (ov-factory (gtk:make-signal-list-item-factory))
         (ov-col (gtk:make-column-view-column :title "OV" :factory ov-factory))
         (abilities-factory (gtk:make-signal-list-item-factory))
         (abilities-col (gtk:make-column-view-column :title "Abilities" :factory abilities-factory)))
    (setf (gtk:column-view-column-sorter chassis-col) (gtk:make-custom-sorter
                                                       :sort-func (cffi:callback compare-string-object-via-accessor)
                                                       :user-data (cffi:make-pointer (glib::put-object (alexandria:compose #'mek/full-name (alexandria:rcurry #'gethash *mul*))))
                                                       :user-destroy (cffi:callback glib::free-object-callback))
          (gtk:column-view-column-sorter role-col) (gtk:make-custom-sorter
                                                       :sort-func (cffi:callback compare-string-object-via-accessor)
                                                       :user-data (cffi:make-pointer (glib::put-object (alexandria:compose #'mek/role (alexandria:rcurry #'gethash *mul*))))
                                                       :user-destroy (cffi:callback glib::free-object-callback))
          (gtk:column-view-column-sorter type-col) (gtk:make-custom-sorter
                                                       :sort-func (cffi:callback compare-string-object-via-accessor)
                                                       :user-data (cffi:make-pointer (glib::put-object (alexandria:compose #'mek/type (alexandria:rcurry #'gethash *mul*))))
                                                       :user-destroy (cffi:callback glib::free-object-callback))
          (gtk:column-view-column-sorter pv-col) (gtk:make-custom-sorter
                                                       :sort-func (cffi:callback compare-string-number-object-via-accessor)
                                                       :user-data (cffi:make-pointer (glib::put-object (alexandria:compose #'mek/pv (alexandria:rcurry #'gethash *mul*))))
                                                       :user-destroy (cffi:callback glib::free-object-callback))
          (gtk:column-view-column-sorter size-col) (gtk:make-custom-sorter
                                                       :sort-func (cffi:callback compare-string-number-object-via-accessor)
                                                       :user-data (cffi:make-pointer (glib::put-object (alexandria:compose #'mek/size (alexandria:rcurry #'gethash *mul*))))
                                                       :user-destroy (cffi:callback glib::free-object-callback))
          (gtk:column-view-column-sorter movement-col) (gtk:make-custom-sorter
                                                       :sort-func (cffi:callback compare-string-object-via-accessor)
                                                       :user-data (cffi:make-pointer (glib::put-object (alexandria:compose #'print-movement (alexandria:rcurry #'gethash *mul*))))
                                                       :user-destroy (cffi:callback glib::free-object-callback))
          (gtk:column-view-column-sorter tmm-col) (gtk:make-custom-sorter
                                                       :sort-func (cffi:callback compare-string-number-object-via-accessor)
                                                       :user-data (cffi:make-pointer (glib::put-object (alexandria:compose #'mek/tmm (alexandria:rcurry #'gethash *mul*))))
                                                       :user-destroy (cffi:callback glib::free-object-callback))
          (gtk:column-view-column-sorter armor-col) (gtk:make-custom-sorter
                                                       :sort-func (cffi:callback compare-string-number-object-via-accessor)
                                                       :user-data (cffi:make-pointer (glib::put-object (alexandria:compose #'mek/armor (alexandria:rcurry #'gethash *mul*))))
                                                       :user-destroy (cffi:callback glib::free-object-callback))
          (gtk:column-view-column-sorter struct-col) (gtk:make-custom-sorter
                                                       :sort-func (cffi:callback compare-string-number-object-via-accessor)
                                                       :user-data (cffi:make-pointer (glib::put-object (alexandria:compose #'mek/structure (alexandria:rcurry #'gethash *mul*))))
                                                       :user-destroy (cffi:callback glib::free-object-callback))
          ;; (gtk:column-view-column-sorter threshold-col) (gtk:make-custom-sorter
          ;;                                              :sort-func (cffi:callback compare-string-number-object-via-accessor)
          ;;                                              :user-data (cffi:make-pointer (glib::put-object (alexandria:compose #'mek/threshold (alexandria:rcurry #'gethash *mul*))))
          ;;                                              :user-destroy (cffi:callback glib::free-object-callback))
          (gtk:column-view-column-sorter short-col) (gtk:make-custom-sorter
                                                       :sort-func (cffi:callback compare-string-number-object-via-accessor)
                                                       :user-data (cffi:make-pointer (glib::put-object (alexandria:compose #'mek/comparable-short (alexandria:rcurry #'gethash *mul*))))
                                                       :user-destroy (cffi:callback glib::free-object-callback))
          (gtk:column-view-column-sorter medium-col) (gtk:make-custom-sorter
                                                       :sort-func (cffi:callback compare-string-number-object-via-accessor)
                                                       :user-data (cffi:make-pointer (glib::put-object (alexandria:compose #'mek/comparable-medium (alexandria:rcurry #'gethash *mul*))))
                                                       :user-destroy (cffi:callback glib::free-object-callback))
          (gtk:column-view-column-sorter long-col) (gtk:make-custom-sorter
                                                       :sort-func (cffi:callback compare-string-number-object-via-accessor)
                                                       :user-data (cffi:make-pointer (glib::put-object (alexandria:compose #'mek/comparable-long (alexandria:rcurry #'gethash *mul*))))
                                                       :user-destroy (cffi:callback glib::free-object-callback))
          (gtk:column-view-column-sorter extreme-col) (gtk:make-custom-sorter
                                                       :sort-func (cffi:callback compare-string-number-object-via-accessor)
                                                       :user-data (cffi:make-pointer (glib::put-object (alexandria:compose #'mek/comparable-extreme (alexandria:rcurry #'gethash *mul*))))
                                                       :user-destroy (cffi:callback glib::free-object-callback))
          (gtk:column-view-column-sorter ov-col) (gtk:make-custom-sorter
                                                       :sort-func (cffi:callback compare-string-number-object-via-accessor)
                                                       :user-data (cffi:make-pointer (glib::put-object (alexandria:compose #'mek/ov (alexandria:rcurry #'gethash *mul*))))
                                                       :user-destroy (cffi:callback glib::free-object-callback))
          (gtk:column-view-column-sorter abilities-col) (gtk:make-custom-sorter
                                                       :sort-func (cffi:callback compare-string-object-via-accessor)
                                                       :user-data (cffi:make-pointer (glib::put-object (alexandria:compose #'mek/abilities (alexandria:rcurry #'gethash *mul*))))
                                                       :user-destroy (cffi:callback glib::free-object-callback)))
    (gtk:column-view-append-column view chassis-col)
    (gtk:column-view-append-column view role-col)
    (gtk:column-view-append-column view type-col)
    (gtk:column-view-append-column view pv-col)
    (gtk:column-view-append-column view size-col)
    (gtk:column-view-append-column view movement-col)
    (gtk:column-view-append-column view tmm-col)
    (gtk:column-view-append-column view armor-col)
    (gtk:column-view-append-column view struct-col)
    ;; (gtk:column-view-append-column view threshold-col)
    (gtk:column-view-append-column view short-col)
    (gtk:column-view-append-column view medium-col)
    (gtk:column-view-append-column view long-col)
    (gtk:column-view-append-column view extreme-col)
    (gtk:column-view-append-column view ov-col)
    (gtk:column-view-append-column view abilities-col)
    (flet ((setup (factory item)
             (declare (ignore factory))
             (setf (gtk:list-item-child item) (gtk:make-label :str "")))
           (unbind (factory item) (declare (ignore factory item)))
           (teardown (factory item) (declare (ignore factory item))))
      (loop :for factory in (list chassis-factory role-factory type-factory
                                  pv-factory size-factory movement-factory tmm-factory
                                  armor-factory struct-factory ;; threshold-factory
                                  short-factory medium-factory long-factory extreme-factory
                                  ov-factory abilities-factory)
            :for accessor in (list #'mek/full-name #'mek/role #'mek/type
                                   #'mek/pv #'mek/size #'print-movement #'mek/tmm
                                   #'mek/armor #'mek/structure ;; #'mek/threshold
                                   #'mek/short-str #'mek/medium-str #'mek/long-str #'mek/extreme-str
                                   #'mek/ov #'mek/abilities)
            :do (gtk:connect factory "setup" #'setup)
                (gtk:connect factory "unbind" #'unbind)
                (gtk:connect factory "teardown" #'teardown)
                (gtk:connect factory "bind"
                             (let ((accessor accessor))
                               (lambda (factory item)
                                 (declare (ignore factory))
                                 (let* ((uuid (gtk:string-object-string (gobj:coerce (gtk:list-item-item item) 'gtk:string-object)))
                                        (row (gethash uuid *mul*))
                                        (value (format nil "~a" (funcall accessor row))))
                                   (setf (gtk:label-text (gobj:coerce (gtk:list-item-child item) 'gtk:label)) value)))))))
    view))
