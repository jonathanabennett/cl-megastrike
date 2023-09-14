(in-package :megastrike)

;; CALLBACK FUNCTIONS for cffi access

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
  ((uuids :initarg :uuids
          :initform '()
          :accessor string-list/strings)
   (source :initarg :source
           :initform nil
           :accessor string-list/source)
   (selected :initarg :selected
             :initform nil
             :accessor string-list/selected)
   (filter :initarg :filter
           :accessor string-list/filter)
   (model :initarg :model
          :initform nil
          :accessor string-list/model)
   (view :initarg :view
         :initform nil
         :accessor string-list/view)))

(defun create-string-list (source &key (filter-object nil) (filter-func nil))
  (let* ((uuids (loop :for u being the hash-keys of source
                      :collect u))
         (model (gtk:make-string-list :strings uuids))
         (view (gtk:make-column-view :model nil))
         (sl (make-instance 'string-list :filter filter-object :uuids uuids :model model
                            :view view :source source)))
    (if filter-func
        (let ((filt (gtk:make-custom-filter
                     :match-func (cffi:callback filter-string-object-via-accessor)
                     :user-data (cffi:make-pointer (glib::put-object (alexandria:compose (alexandria:curry filter-func (string-list/filter sl)) (alexandria:rcurry #'gethash (string-list/source sl)))))
                     :user-destroy (cffi:callback glib::free-object-callback))))
          (setf (gtk:column-view-model view)
                (gtk:make-single-selection :model (gtk:make-sort-list-model :model (gtk:make-filter-list-model :model model :filter filt) :sorter (gtk:column-view-sorter view)))))
        (setf (gtk:column-view-model view)
              (gtk:make-single-selection :model (gtk:make-sort-list-model :model model :sorter (gtk:column-view-sorter view)))))
    (gtk:connect (gtk:column-view-model view) "selection-changed"
                   (lambda (model position n-items)
                     (declare (ignore position n-items))
                     (let ((uuid (gtk:string-object-string (gobj:coerce (gtk:single-selection-selected-item model) 'gtk:string-object))))
                       (setf (string-list/selected sl) (gethash uuid source nil)))))
    sl))

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
    (if (not (= (hash-table-count (string-list/source sl)) (length (string-list/strings sl))))
        (progn
          (add-to-end (string-list/strings sl) uuid)
          (gtk:string-list-append (string-list/model sl) uuid)))))

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

;;   (defun force-pv-update ()
;;     ;; Check which item is select and select the opposite.
;;     (let* ((m (gobj:coerce (gtk:column-view-model view) 'gtk:single-selection))
;;            (pos (gtk:single-selection-selected m)))
;;       (setf (gtk:single-selection-can-unselect-p m) t)
;;       (gtk:selection-model-selection-changed m (+ ) 1)))
