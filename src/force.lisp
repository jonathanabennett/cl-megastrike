(in-package :megastrike)

(defclass force ()
  ((name        :initarg :name   :accessor force/name)
   (color       :initarg :color  :accessor force/color)
   (deploy-zone :initarg :deploy :accessor force/deployment)
   (units       :initarg :units  :accessor force/units)
   (initiative  :initform nil    :accessor force/initiative)))

(defun new-force (name color deploy &optional (unit-list '()))
  (let ((f (make-instance 'force
                       :name name
                       :color color
                       :deploy deploy
                       :units unit-list)))
    f))

(defmethod same-force ((f force) (o force))
  (string= (force/name f) (force/name o)))

(defmethod same-force ((f force) (s string))
  (string= (force/name f) s))

(defmethod same-force ((s string) (f force))
  (string= (force/name f) s))

(defmethod same-force ((f force) o)
  nil)

(defmethod same-force (o (f force))
  nil)

(defmethod add-unit ((f force) (u combat-unit))
  (setf (info/force u) f))

(defmethod count-units ((f force))
  (let ((count 0))
    (map-entities #'(lambda (e) (if (same-force (info/force e) f) (incf count))))
    count))

(defmethod force-pv ((f force))
  (let ((total 0))
    (map-entities #'(lambda (e)
                      (if (same-force (info/force e) f) (incf total (info/pv e)))))
    total))

(defmethod turn-order-list ((f force))
  (let ((force-string (force/name f))
        (order '()))
  (dotimes (i (count-units f))
    (push force-string order))
    order))

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
