(in-package :megastrike)

;:; CALLBACK FUNCTIONS for cffi access
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

(defclass string-list ()
  ((uuids :accessor string-list/strings
          :initarg :uuids
          :documentation "The UUID strings used as keys in the source and as the strings in the model.")
   (source :accessor string-list/source
           :initarg :source
           :documentation "The hash-table which holds the objects to be displayed.")
   (selected :accessor string-list/selected
             :initarg :selected
             :initform nil
             :documentation "The currently selected object in the list store.")
   (filter-object :accessor string-list/filter-object
                  :initarg :filter-object
                  :initform nil
                  :documentation "The object used as a filter.")
   (filter :accessor string-list/filter
           :initarg :filter
           :initform nil
           :documentation "The GTK:CustomFilter object")
   (model :accessor string-list/model
          :initarg :model
          :documentation "The GTK:StringListStore object")
   (view :accessor string-list/view
         :initarg :view
         :documentation "The GTK:ColumnView object"))
  (:documentation "A helper class to manage interactions with GTK:StringListStores."))

(defun create-string-list (source &key (filter-object nil) (filter-func nil))
  (let* ((uuids (loop :for u being the hash-keys of source
                      :collect u))
         (model (gtk:make-string-list :strings uuids))
         (view (gtk:make-column-view :model nil))
         (sl (make-instance 'string-list :uuids uuids :model model :view view :source source)))
    (setf (gtk:column-view-model view)
          (gtk:make-single-selection :model (gtk:make-sort-list-model :model model :sorter (gtk:column-view-sorter view))))
    (when (and filter-object filter-func)
        (string-list/add-filter sl filter-object filter-func))
    (gtk:connect (gtk:column-view-model view) "selection-changed"
                   (lambda (model position n-items)
                     (declare (ignore position n-items))
                     (let ((uuid (gtk:string-object-string (gobj:coerce (gtk:single-selection-selected-item model) 'gtk:string-object))))
                       (setf (string-list/selected sl) (gethash uuid source nil)))))
    sl))

(defmethod string-list/add-filter ((sl string-list) filter-object filter-func)
  (setf (string-list/filter-object sl) filter-object)
  (setf (string-list/filter sl) (gtk:make-custom-filter
                                 :match-func (cffi:callback filter-string-object-via-accessor)
                                 :user-data (cffi:make-pointer (glib::put-object (alexandria:compose (alexandria:curry filter-func (string-list/filter-object sl)) (alexandria:rcurry #'gethash (string-list/source sl)))))
                                 :user-destroy (cffi:callback glib::free-object-callback)))
  (setf (gtk:column-view-model (string-list/view sl))
        (gtk:make-single-selection :model (gtk:make-sort-list-model :model (gtk:make-filter-list-model :model (string-list/model sl) :filter (string-list/filter sl))
                                                                    :sorter (gtk:column-view-sorter (string-list/view sl))))))

(defmethod string-list/add-label-column ((sl string-list) title accessor datatype comparator)
  "Label columns sort by default."
  (let* ((fact (gtk:make-signal-list-item-factory))
         (col (gtk:make-column-view-column :title title :factory fact)))
    (gtk:column-view-append-column (string-list/view sl) col)
    (setf (gtk:column-view-column-sorter col) (gtk:make-custom-sorter
                                               :sort-func (if (string= datatype "string")
                                                              (cffi:callback compare-string-object-via-accessor)
                                                              (cffi:callback compare-string-number-object-via-accessor))
                                               :user-data (cffi:make-pointer (glib::put-object (alexandria:compose comparator (alexandria:rcurry #'gethash (string-list/source sl)))))
                                               :user-destroy (cffi:callback glib::free-object-callback)))
    (gtk:connect fact "setup" (lambda (factory item)
                                (declare (ignore factory))
                                (setf (gtk:list-item-child item) (gtk:make-label :str ""))))
    (gtk:connect fact "unbind" (lambda (factory item) (declare (ignore factory item))))
    (gtk:connect fact "teardown" (lambda (factory item) (declare (ignore factory item))))
    (gtk:connect fact "bind"
                 (lambda (factory item)
                   (declare (ignore factory))
                   (let* ((uuid (gtk:string-object-string (gobj:coerce (gtk:list-item-item item) 'gtk:string-object)))
                          (row (gethash uuid (string-list/source sl)))
                          (value (format nil "~a" (funcall accessor row))))
                     (setf (gtk:label-text (gobj:coerce (gtk:list-item-child item) 'gtk:label)) value))))))

(defmethod string-list/add-color-column ((sl string-list) title accessor)
  (let* ((fact (gtk:make-signal-list-item-factory))
         (col (gtk:make-column-view-column :title title :factory fact)))
    (gtk:column-view-append-column (string-list/view sl) col)
    (gtk:connect fact "setup" (lambda (factory item)
                                (declare (ignore factory))
                                (setf (gtk:list-item-child item) (gtk:make-color-dialog-button :dialog (gtk:make-color-dialog)))))
    (gtk:connect fact "unbind" (lambda (factory item) (declare (ignore factory item))))
    (gtk:connect fact "teardown" (lambda (factory item) (declare (ignore factory item))))
    (gtk:connect fact "bind"
                 (lambda (factory item)
                   (declare (ignore factory))
                   (let* ((uuid (gtk:string-object-string (gobj:coerce (gtk:list-item-item item) 'gtk:string-object)))
                          (row (gethash uuid (string-list/source sl)))
                          (value (format nil "~a" (funcall accessor row))))
                     (with-gdk-rgba (color value)
                       (setf (gtk:color-dialog-button-rgba (gobj:coerce (gtk:list-item-child item) 'gtk:color-dialog-button)) color)))))))

(defmethod string-list/add-item ((sl string-list) item item-name)
  (let ((uuid (format nil "~a" (uuid:make-v5-uuid uuid:+namespace-dns+ item-name))))
    (setf (gethash uuid (string-list/source sl)) item)
    (unless (= (hash-table-count (string-list/source sl)) (length (string-list/strings sl)))
      (add-to-end (string-list/strings sl) uuid)
      (gtk:string-list-append (string-list/model sl) uuid))))

(defmethod string-list/get-column-by-title ((sl string-list) title)
  (remove-if-not #'column-searcher (gobj:coerce (gtk:column-view-columns (string-list/view sl)) 'gtk:string-object-string) title))

(defun column-searcher (column search-term)
  (string= (gtk:column-view-column-title column) search-term))

(defmethod string-list/brute-update ((sl string-list))
  "This update function is only useful for short lists as it manually wipes the list clean and repopulates it."
  (let* ((model (gobj:coerce (gtk:column-view-model (string-list/view sl)) 'gtk:single-selection))
         (pos (gtk:single-selection-selected model)))
    (mapcar (lambda (el) (declare (ignore el))
              (gtk:string-list-remove (string-list/model sl) 0))
            (string-list/strings sl))
    (mapcar (lambda (el)
            (gtk:string-list-append (string-list/model sl) el))
            (string-list/strings sl))
    (gtk:selection-model-select-item model pos t)))

(defmethod string-list/smart-update ((sl string-list))
  ;; Still not working
  (let* ((model (gobj:coerce (gtk:column-view-model (string-list/view sl)) 'gtk:single-selection))
         (pos (gtk:single-selection-selected model)))
    (gtk:selection-model-selection-changed model pos 1)
    (gio:list-model-items-changed model pos 0 0)))
