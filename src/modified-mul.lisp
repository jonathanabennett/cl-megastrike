(cffi:defcallback compare-string-object-via-accessor :int
    ((a :pointer)
     (b :pointer)
     (data :pointer))
  (let ((accessor (gethash (cffi:pointer-address data) glib::*objects*))) ; Use (glib::get-object (cffi:pointer-address data)) in the latest version of cl-glib.
    (let ((string-a (funcall accessor (gtk:string-object-string (gobj:pointer-object a 'gtk:string-object))))
          (string-b (funcall accessor (gtk:string-object-string (gobj:pointer-object b 'gtk:string-object)))))
      (if (string< string-a string-b) +1 -1))))

(cffi:defcallback compare-string-number-object-via-accessor :int
    ((a :pointer)
     (b :pointer)
     (data :pointer))
  (let ((accessor (gethash (cffi:pointer-address data) glib::*objects*))) ; Use (glib::get-object (cffi:pointer-address data)) in the latest version of cl-glib.
    (let ((number-a (funcall accessor (gtk:string-object-string (gobj:pointer-object a 'gtk:string-object))))
          (number-b (funcall accessor (gtk:string-object-string (gobj:pointer-object b 'gtk:string-object)))))
      (if (< number-a number-b) +1 -1))))

(defparameter *mul* (make-hash-table :test #'equal))

(defun mul-list-view ()
  (let* ((model (gtk:make-string-list :strings (loop :for uuid :being :the hash-keys :of *mul* :collect uuid)))
         (view (gtk:make-column-view :model nil))
         (chassis-factory (gtk:make-signal-list-item-factory))
         (chassis-col (gtk:make-column-view-column :title "Chassis" :factory chassis-factory)))
    (setf (gtk:column-view-column-sorter chassis-col) (gtk:make-custom-sorter :sort-func (cffi:callback compare-string-object-via-accessor)
                                                                       :user-data (cffi:make-pointer
                                                                                   (glib::put-object
                                                                                    (alexandria:rcurry
                                                                                     #'gethash *mul*)))
                                                                       :user-destroy (cffi:callback glib::free-object-callback)))
    (gtk:column-view-append-column view chassis-col)
    (setf (gtk:column-view-model view) (gtk:make-single-selection :model (gtk:make-sort-list-model :model model :sorter (gtk:column-view-sorter view))))
    (setf (gtk:widget-vexpand-p view) t
          (gtk:widget-hexpand-p view) t)
    (flet ((setup (factory item)
             (declare (ignore factory))
             (setf (gtk:list-item-child item) (gtk:make-label :str "")))
           (unbind (factory item) (declare (ignore factory item)))
           (teardown (factory item) (declare (ignore factory item))))
      (loop :for factory :in (list chassis-factory)
            :for accessor :in (list #'princ-to-string)
            :do (gtk:connect factory "setup" #'setup)
                (gtk:connect factory "unbind" #'unbind)
                (gtk:connect factory "teardown" #'teardown)
                (gtk:connect factory "bind"
                             (let ((accessor accessor))
                               (lambda (factory item)
                                 (declare (ignore factory))
                                 (let* ((uuid (gtk:string-object-string (gobj:coerce (gtk:list-item-item item) 'gtk:string-object)))
                                        (row (gethash uuid *mul*)))
                                   (setf (gtk:label-text (gobj:coerce (gtk:list-item-child item) 'gtk:label)) (funcall accessor row))))))))
    view))

(gtk:define-application (:name issue-25
                         :id "org.bohonghuang.cl-gtk4.issue-25")
  (gtk:define-main-window (window (gtk:make-application-window :application gtk:*application*))
    (setf *mul* (make-hash-table :test #'equal))
    (loop :for i :from 1 :to 10
          :do (setf (gethash (princ-to-string (uuid:make-v1-uuid)) *mul*) (format nil "Element ~A" i)))
    (setf (gtk:window-title window) "List Model Test"
          (gtk:window-child window) (mul-list-view))
    (unless (gtk:widget-visible-p window)
      (gtk:window-present window))))
