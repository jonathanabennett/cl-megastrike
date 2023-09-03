(in-package :megastrike)

(defvar *mul* (make-hash-table :test #'equal))

;; This will read in the data from the MUL.csv file after it's cleaned up
;; (cl-csv:read-csv #p"data/units/mul.csv" :separator #\Tab :escape-mode :following)
;; Cleaning up is accomplished by:
;; 1) Deleting the first to rows (the instructions from Megamek on how to regenerate the file
;; 2) Running :%s/\(\d\+\)"/\1""/g in VIM on the file.
;; To regeneratoe from Megamek: java -jar MegaMek.jar -asc filename.
;; Copy it into the data/units folder of Megastrike

(defclass mek ()
  ((chassis   :initarg :chassis   :initform "" :accessor mek/chassis)
   (model     :initarg :model     :initform "" :accessor mek/model)
   (role      :initarg :role      :initform nil :accessor mek/role)
   (unit-type :initarg :type      :initform nil :accessor mek/type)
   (size      :initarg :size      :initform 0   :accessor mek/size)
   (movement  :initarg :movement  :initform ""  :accessor mek/movement)
   (tmm       :initarg :tmm       :initform 0   :accessor mek/tmm
              :documentation "TMM from the distance only of the first movement type. Does not include movement type modifier like jumping")
   (armor     :initarg :armor     :initform 0   :accessor mek/armor)
   (structure :initarg :structure :initform 0   :accessor mek/structure)
   (threshold :initarg :threshold :initform 0   :accessor mek/threshold)
   (short     :initarg :short     :initform 0   :accessor mek/short)
   (short*    :initarg :short*    :initform nil :accessor mek/short*)
   (medium    :initarg :medium    :initform 0   :accessor mek/medium)
   (medium*   :initarg :medium*   :initform nil :accessor mek/medium*)
   (long      :initarg :long      :initform 0   :accessor mek/long)
   (long*     :initarg :long*     :initform nil :accessor mek/long*)
   (extreme   :initarg :extreme   :initform 0   :accessor mek/extreme)
   (extreme*  :initarg :extreme*  :initform nil :accessor mek/extreme*)
   (ov        :initarg :ov        :initform 0   :accessor mek/ov)
   (pv        :initarg :pv        :initform 0   :accessor mek/pv)
   (display   :initarg :display   :initform nil :accessor mek/display)
   (abilities :initarg :abilities :initform ""  :accessor mek/abilities)
   (front-arc :initarg :front-arc :initform nil :accessor mek/front-arc)
   (left-arc  :initarg :left-arc  :initform nil :accessor mek/left-arc)
   (right-arc :initarg :right-arc :initform nil :accessor mek/right-arc)
   (rear-arc  :initarg :rear-arc  :initform nil :accessor mek/rear-arc)))

(defun new-mek (chassis model role unit-type size movement tmm armor structure
                threshold short short* medium medium* long long* extreme extreme*
                ov pv abilities front-arc left-arc right-arc rear-arc)
   (make-instance 'mek :chassis chassis :model model :role role :type unit-type
                       :size size :movement movement :tmm tmm :armor armor
                       :structure structure :threshold threshold
                       :short short :short* short* :medium medium :medium* medium*
                       :long long :long* long* :extreme extreme :extreme* extreme*
                       :ov ov :pv pv :abilities abilities :front-arc front-arc
                       :left-arc left-arc :right-arc right-arc :rear-arc rear-arc))

(defun mul-parser (header-row row)
  (let ((uuid (format nil "~a" (uuid:make-v1-uuid)))
        (data (loop for key in header-row
                    for value in row
                    collect `(,(read-from-string key) . ,value))))
    (setf (cdr (assoc 'size data)) (parse-integer (cdr (assoc 'size data))))
    (setf (cdr (assoc 'movement data)) (construct-mv-alist (cdr (assoc 'movement data))))
    (setf (cdr (assoc 'tmm data)) (parse-integer (cdr (assoc 'tmm data))))
    (setf (cdr (assoc 'armor data)) (parse-integer (cdr (assoc 'armor data))))
    (setf (cdr (assoc 'structure data)) (parse-integer (cdr (assoc 'structure data))))
    (setf (cdr (assoc 'threshold data)) (parse-integer (cdr (assoc 'threshold data))))
    (setf (cdr (assoc 's data)) (parse-integer (cdr (assoc 's data))))
    (setf (cdr (assoc 's* data)) (string= (cdr (assoc 's* data)) "TRUE"))
    (setf (cdr (assoc 'm data)) (parse-integer (cdr (assoc 'm data))))
    (setf (cdr (assoc 'm* data)) (string= (cdr (assoc 'm* data)) "TRUE"))
    (setf (cdr (assoc 'l data)) (parse-integer (cdr (assoc 'l data))))
    (setf (cdr (assoc 'l* data)) (string= (cdr (assoc 'l* data)) "TRUE"))
    (setf (cdr (assoc 'e data)) (parse-integer (cdr (assoc 'e data))))
    (setf (cdr (assoc 'e* data)) (string= (cdr (assoc 'e* data)) "TRUE"))
    (setf (cdr (assoc 'overheat data)) (parse-integer (cdr (assoc 'overheat data))))
    (setf (cdr (assoc 'point data)) (parse-integer (cdr (assoc 'point data))))
    (let ((uuid (format nil "~a" (uuid:make-v5-uuid uuid::+namespace-dns+ (format nil "~a ~a" (cdr (assoc 'chassis data)) (cdr (assoc 'model data)))))))
      (setf (gethash uuid *mul*)
            (new-mek (cdr (assoc 'chassis data)) (cdr (assoc 'model data))
                     (cdr (assoc 'role data)) (cdr (assoc 'type data))
                     (cdr (assoc 'size data)) (cdr (assoc 'movement data))
                     (cdr (assoc 'tmm data)) (cdr (assoc 'armor data))
                     (cdr (assoc 'structure data)) (cdr (assoc 'threshold data))
                     (cdr (assoc 's data)) (cdr (assoc 's* data))
                     (cdr (assoc 'm data)) (cdr (assoc 'm* data))
                     (cdr (assoc 'l data)) (cdr (assoc 'l* data))
                     (cdr (assoc 'e data)) (cdr (assoc 'e* data))
                     (cdr (assoc 'overheat data)) (cdr (assoc 'point data))
                     (cdr (assoc 'abilities data)) (cdr (assoc 'front-arc data))
                     (cdr (assoc 'left-arc data)) (cdr (assoc 'right-arc data))
                     (cdr (assoc 'rear-arc data)))))))

(defmacro mek/comparable-num (func mek)
  (if mek
      (let ((fname (gensym))
            (m (gensym))
            (val (gensym)))
        `(let* ((,fname ,func)
                (,m ,mek)
                (,val (funcall ,fname ,m)))
           (if ,val
               ,val
               -1)))
      -1))

(defun mek/comparable-ov (m)
  (if m
      (if (mek/ov m)
          (mek/ov m)
          -1)
      -1))

(defun mek/full-name (m)
  (if m
      (format nil "~a ~a" (mek/chassis m) (mek/model m))
      ""))

(defun mek/comparable-short (m)
  (if m
      (if (mek/short* m)
          0.5
          (mek/short m))
      0))

(defun mek/comparable-medium (m)
  (if m
      (if (mek/medium* m)
          0.5
          (mek/medium m))
      0))

(defun mek/comparable-long (m)
  (if m
      (if (mek/long* m)
          0.5
          (mek/long m))
      0))

(defun mek/comparable-extreme (m)
  (if m
      (if (mek/extreme* m)
          0.5
          (mek/extreme m))
      0))

(defun mek/short-str (m)
  (if m
      (if (mek/short* m)
          "0*"
          (format nil "~a" (mek/short m)))
      ""))

(defun mek/medium-str (m)
  (if m
      (if (mek/medium* m)
          "0*"
          (format nil "~a" (mek/medium m)))
      ""))

(defun mek/long-str (m)
  (if m
      (if (mek/long* m)
          "0*"
          (format nil "~a" (mek/long m)))
      ""))

(defun mek/extreme-str (m)
  (if m
      (if (mek/extreme* m)
          "0*"
          (format nil "~a" (mek/extreme m)))
      ""))

(defun construct-mv-alist (mv-string)
  (let ((mv-alist '())
         (mv-strings (ppcre:all-matches-as-strings
                      (ppcre:create-scanner "\\d+\\\"[a-zA-z]?") mv-string)))
    (dolist (str mv-strings)
      (multiple-value-bind (dist type) (extract-numbers-and-letters str)
        (setf mv-alist (acons (cdr (rassoc type *mv-designators* :test #'string=))
                              (/ (parse-integer dist) 2) mv-alist))))
    mv-alist))

(defun format-move-assoc (stream m colonp atsignp)
  "The colonp and atsignp are required for a function called inside `FORMAT'."
  (declare (ignorable colonp atsignp))
  (format stream "~a~a" (cdr m) (cdr (rassoc (car m) *mv-designators* :test #'string=))))

(defun print-movement (m)
  (if m
      (format nil "~{~/megastrike::format-move-assoc/~^/~}" (mek/movement m))
      "None"))

(defun filter-mek (filter m)
  (let ((filt-name (or (mek/chassis filter) nil))
        (filt-type (or (mek/type filter) nil)))
    (and (if filt-name (search filt-name (mek/full-name m)) t)
         (if filt-type (member (mek/type m) filt-type :test #'string=) t)
         )))

(defun extract-numbers-and-letters (input)
  (let ((number-part "")
        (letter-part ""))
    (loop for char across input
          if (digit-char-p char)
          do (setf number-part (concatenate 'string number-part (string char)))
          else if (alpha-char-p char)
          do (setf letter-part (concatenate 'string letter-part (string char))))
    (values number-part letter-part)))

(defun load-mul (f)
  (let* ((data (cl-csv:read-csv f :separator #\Tab :escape-mode :following))
         (h-row (nth 0 data)))
    (dolist (r (cdr data))
      (mul-parser h-row r))))

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

(cffi:defcallback filter-string-object-via-accessor :bool
    ((item :pointer)
     (data :pointer))
  (let ((filter-func (gethash (cffi:pointer-address data) glib::*objects*)))
    (funcall filter-func (gtk:string-object-string (gobj:pointer-object item 'gtk:string-object)))))

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
                     (setf pskill-entry (ignore-errors
                                         (gtk-entry-buffer-text (gtk:entry-buffer pskill-entry))))))
      (gtk:connect new-unit-btn "clicked"
                   (lambda (button)
                     (declare (ignore button))
                     (let ((skill (parse-integer pskill :junk-allowed t)))
                       (if (and pname skill selected)
                           (let ((el (new-element-from-mul selected pname skill)))
                             (add-unit (game/selected-force *game*) el))))))
      (gtk:grid-attach layout pname-label  0 3 1 1)
      (gtk:grid-attach layout pname-entry  1 3 1 1)
      (gtk:grid-attach layout pskill-label 2 3 1 1)
      (gtk:grid-attach layout pskill-entry 3 3 1 1)
      (gtk:grid-attach layout new-unit-btn 4 3 1 1)
      )
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
         (movement-col (gtk:make-column-view-column :title "Movement" :factory movement-factory))
         (tmm-factory (gtk:make-signal-list-item-factory))
         (tmm-col (gtk:make-column-view-column :title "TMM" :factory tmm-factory))
         (armor-factory (gtk:make-signal-list-item-factory))
         (armor-col (gtk:make-column-view-column :title "Armor" :factory armor-factory))
         (struct-factory (gtk:make-signal-list-item-factory))
         (struct-col (gtk:make-column-view-column :title "Structure" :factory struct-factory))
         (threshold-factory (gtk:make-signal-list-item-factory))
         (threshold-col (gtk:make-column-view-column :title "Threshold" :factory threshold-factory))
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
          (gtk:column-view-column-sorter threshold-col) (gtk:make-custom-sorter
                                                       :sort-func (cffi:callback compare-string-number-object-via-accessor)
                                                       :user-data (cffi:make-pointer (glib::put-object (alexandria:compose #'mek/threshold (alexandria:rcurry #'gethash *mul*))))
                                                       :user-destroy (cffi:callback glib::free-object-callback))
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
    (gtk:column-view-append-column view threshold-col)
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
                                  armor-factory struct-factory threshold-factory
                                  short-factory medium-factory long-factory extreme-factory
                                  ov-factory abilities-factory)
            :for accessor in (list #'mek/full-name #'mek/role #'mek/type
                                   #'mek/pv #'mek/size #'print-movement #'mek/tmm
                                   #'mek/armor #'mek/structure #'mek/threshold
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
