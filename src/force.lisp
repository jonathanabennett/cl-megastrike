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

(defmethod add-force ((g game) (f force))
  (push f (game/forces g)))

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

(let ((col-force-name 0) (col-force-color 1) (col-force-deployment 2)
      (col-force-pv 3) (m nil) (view nil))

  (defun build-force-model ()
    (let ((model (make-instance 'gtk-list-store
                            :column-types '("gchararray" "gchararray"
                                            "gchararray" "gint"))))
      (dolist (f (game/forces *game*))
        (let ((iter (gtk-list-store-append model)))
          (gtk-list-store-set model
                              iter
                              (force/name f)
                              (gdk-rgba-to-string (force/color f))
                              (force/deployment f)
                              (force-pv f))))
      (setf m model)))

(defun update-force (model path iter)
  (let* ((name (gtk-tree-model-get-value model iter 0))
         (pv (gtk-tree-model-get-value model iter 3))
         (force (car (member name (game/forces *game*) :test #'same-force))))
    (gtk-list-store-set-value model iter 3 (force-pv force))))

  (defun add-new-force (force)
    (add-force *game* force)
    (gtk-list-store-set m
                        (gtk-list-store-append m)
                        (force/name force)
                        (gdk-rgba-to-string (force/color force))
                        (force/deployment force)
                        (force-pv force)))

  (defun force-color-cell-data (column renderer model iter)
    (declare (ignore column))
    (let ((rgba-string (gtk-tree-model-get-value model iter col-force-color)))
      (setf (gtk-cell-renderer-text-background-rgba renderer) (gdk-rgba-parse rgba-string))
      (setf (gtk-cell-renderer-text-text renderer) rgba-string)))

  (defun update-forces ()
    (gtk-tree-model-foreach m #'update-force))

  (defun build-force-view ()
    (let ((v (gtk-tree-view-new-with-model m)))
      (let* ((renderer (gtk-cell-renderer-text-new))
             (column (gtk-tree-view-column-new-with-attributes "Name"
                                                               renderer
                                                               "text"
                                                               col-force-name)))
        (gtk-tree-view-append-column v column))
      (let* ((renderer (gtk-cell-renderer-text-new))
             (column (gtk-tree-view-column-new-with-attributes "Color"
                                                               renderer
                                                               "text"
                                                               col-force-color)))
        (gtk-tree-view-column-set-cell-data-func column renderer #'force-color-cell-data)
        (gtk-tree-view-append-column v column))
      (let* ((renderer (gtk-cell-renderer-text-new))
             (column (gtk-tree-view-column-new-with-attributes "Deployment Zone"
                                                               renderer
                                                               "text"
                                                               col-force-deployment)))
        (gtk-tree-view-append-column v column))
      (let* ((renderer (gtk-cell-renderer-text-new))
             (column (gtk-tree-view-column-new-with-attributes "PV"
                                                               renderer
                                                               "text"
                                                               col-force-pv)))
        (gtk-tree-view-append-column v column))
      (setf view v)))

  (defun draw-force-setup (window)
    (let* ((layout (make-instance 'gtk-box
                                  :orientation :vertical
                                  :spacing 10))
           (title (make-instance 'gtk-label
                                 :use-markup t
                                 :label "<big>Force Setup</big>"))
           (force-builder-row1 (make-instance 'gtk-box
                                             :orientation :horizontal
                                             :spacing 10))
           (force-builder-row2 (make-instance 'gtk-box
                                             :orientation :horizontal
                                             :spacing 10))
           (new-force-label (make-instance 'gtk-label
                                           :label "Force Name: "))
           (new-force-entry (make-instance 'gtk-entry
                                           :width-chars 20))
           (new-force-color (make-instance 'gtk-color-button
                                           :rgba (gdk-rgba-parse "Gold")))
           (new-deploy-label (make-instance 'gtk-label
                                            :label "Deployment Zone: "))
           (new-deploy-entry (make-instance 'gtk-entry
                                            :width-chars 20))
           (new-force-button (gtk-button-new-with-label "New Force")))
      (build-force-model)
      (build-force-view)
      (g-signal-connect new-force-button "clicked"
                        (lambda (widget)
                          (declare (ignore widget))
                          (let ((name (gtk-entry-text new-force-entry))
                                (deploy (gtk-entry-text new-deploy-entry))
                                (color (gtk-color-button-rgba new-force-color)))
                            (if (and name deploy color)
                                (progn
                                  (add-new-force (new-force name color deploy)))))
                          (gtk-widget-queue-draw window)))
      (let ((selection (gtk-tree-view-get-selection view)))
        (setf (gtk-tree-selection-mode selection) :single)
        (g-signal-connect selection "changed"
                          (lambda (object)
                            (let* ((view (gtk-tree-selection-get-tree-view object))
                                   (model (gtk-tree-view-model view))
                                   (iter (gtk-tree-selection-get-selected object))
                                   (name (gtk-tree-model-get-value model iter col-force-name)))
                              (setf (game/selected-force *game*)
                                    (car (member name (game/forces *game*)
                                                 :test #'same-force)))))))
      (gtk-box-pack-start layout title)
      (gtk-box-pack-start force-builder-row1 new-force-label)
      (gtk-box-pack-start force-builder-row1 new-force-entry)
      (gtk-box-pack-start force-builder-row1 new-force-color)
      (gtk-box-pack-start force-builder-row2 new-deploy-label)
      (gtk-box-pack-start force-builder-row2 new-deploy-entry)
      (gtk-box-pack-start force-builder-row2 new-force-button)
      (gtk-box-pack-start layout force-builder-row1)
      (gtk-box-pack-start layout force-builder-row2)
      (gtk-box-pack-start layout view)
      layout)))
