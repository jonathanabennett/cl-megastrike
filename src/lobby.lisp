(in-package :megastrike)

(defclass lobby ()
  ((selected-mek  :initform nil :accessor lobby/selected-mek)
   (game          :initform nil :accessor lobby/game
                  :initarg :game)))
(defun new-lobby ()
  (let ((g (new-game)))
    (make-instance 'lobby :game g)))

(defun draw-lobby-screen ()
  (let ((layout (make-grid))
        (map-selection (draw-map-selection))
        (force-setup (draw-force-setup))
        (unit-selection (draw-unit-selection))
        (unit-list (draw-unit-list)))
    ;; (let ((button (gtk-button-new-with-label "Not Ready")))
    ;;   (connect button "clicked" (lambda (widget)
    ;;                               (declare (ignore widget))
    ;;                               (if (gtk-widget-is-visible layout)
    ;;                                   (progn
    ;;                                     (gtk-container-remove window layout)
    ;;                                     (draw-gameplay-screen window))))))
    ;; (g-timeout-add 500 (lambda ()
    ;;                      (if (and (check-board)
    ;;                               (check-forces)
    ;;                               (check-units))
    ;;                          (progn
    ;;                            (setf (gtk-widget-sensitive launch-button) t)
    ;;                            (setf (gtk-button-label launch-button) "Launch Game")))
    ;;                      t))
    (grid-attach layout map-selection  0 0 1 1)
    (grid-attach layout unit-selection 1 0 1 1)
    (grid-attach layout force-setup    0 1 1 1)
    (grid-attach layout unit-list      1 1 1 1)
    layout))

(defun draw-force-setup ()
  (let ((layout (make-grid))
        (name-label (make-label :str "Unit Selection"))
        (name-entry (make-entry))
        (color-selection (make-color-button))
        (deploy-label (make-label :str "Deployment Zone"))
        (deploy-entry (make-entry)))
    (grid-attach layout name-label 0 0 1 1)
    (grid-attach layout name-entry 1 0 1 1)
    (grid-attach layout color-selection    0 1 1 1)
    (grid-attach layout deploy-label      1 1 1 1)
    (grid-attach layout deploy-entry  2 2 1 1)
    layout))


(defun draw-unit-selection ()
  (let ((layout (make-grid))
        (name-label (make-label :str "Unit List"))
        (name-entry (make-entry))
        (color-selection (make-color-button))
        (deploy-label (make-label :str "Deployment Zone"))
        (deploy-entry (make-entry)))
    (grid-attach layout name-label 0 0 1 1)
    (grid-attach layout name-entry 1 0 1 1)
    (grid-attach layout color-selection    0 1 1 1)
    (grid-attach layout deploy-label      1 1 1 1)
    (grid-attach layout deploy-entry  2 2 1 1)
    layout))

(defun draw-unit-list ()
  (let ((layout (make-grid))
        (name-label (make-label :str "Force Name"))
        (name-entry (make-entry))
        (color-selection (make-color-button))
        (deploy-label (make-label :str "Deployment Zone"))
        (deploy-entry (make-entry)))
    (grid-attach layout name-label 0 0 1 1)
    (grid-attach layout name-entry 1 0 1 1)
    (grid-attach layout color-selection    0 1 1 1)
    (grid-attach layout deploy-label      1 1 1 1)
    (grid-attach layout deploy-entry  2 2 1 1)
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
  (let ((layout (make-grid))
        (title (make-label :str "<big>Map Selection</big>"))
        (width-label (make-label :str "<b>Map Width: </b>"))
        (width-entry (make-entry))
        (height-label (make-label :str "<b>Map Height: </b>"))
        (height-entry (make-entry))
        (map-created (make-label :str "No Map Created.")))
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
    (grid-attach layout title 0 0 2 1)
    (grid-attach layout width-label 0 1 1 1)
    (grid-attach layout width-entry 1 1 1 1)
    (grid-attach layout height-label 2 1 1 1)
    (grid-attach layout height-entry 3 1 1 1)
    (grid-attach layout map-created 0 2 1 1)
    layout))

;; (let ((col-id 0) (col-force 1) (col-full-name 2)
;;       (col-type 3) (col-role 4) (col-pv 5)
;;       (col-mv 6) (col-arm-struct 7) (col-attack 8)
;;       (col-ov 9) (col-specials 10) (model nil))

;;   (defun build-unit-list-model ()
;;     (let ((m (make-instance 'gtk-list-store
;;                             :column-types '("gint" "gchararray" "gchararray"
;;                                             "gchararray" "gchararray" "gint"
;;                                             "gchararray" "gchararray" "gchararray"
;;                                             "gint" "gchararray"))))
;;       m))

;;   (defun add-to-unit-list (cu)
;;     (let ((iter (gtk-list-store-append model)))
;;       (gtk-list-store-set model
;;                           iter
;;                           (entity-id cu)
;;                           (force/name (info/force cu))
;;                           (info/full-name cu)
;;                           (info/unit-type cu)
;;                           (info/role cu)
;;                           (info/pv cu)
;;                           (moveable/format-move cu)
;;                           (format nil "~a/~a" (damageable/cur-armor cu) (damageable/cur-struct cu))
;;                           (format nil "~a/~a/~a"
;;                                   (attacks/short cu)
;;                                   (attacks/medium cu)
;;                                   (attacks/long cu))
;;                           (heat/ov cu)
;;                           (format nil "~{~a, ~}" (specials/special-list cu)))))

;;   (defun build-unit-list-view (model)
;;     (let ((view (gtk-tree-view-new-with-model model)))
;;       (let* ((renderer (gtk-cell-renderer-text-new))
;;              (column (gtk-tree-view-column-new-with-attributes
;;                       "ID" renderer "text" col-id)))
;;         (gtk-tree-view-append-column view column))

;;       (let* ((renderer (gtk-cell-renderer-text-new))
;;              (column (gtk-tree-view-column-new-with-attributes
;;                       "Force" renderer "text" col-force)))
;;         (gtk-tree-view-append-column view column))

;;       (let* ((renderer (gtk-cell-renderer-text-new))
;;              (column (gtk-tree-view-column-new-with-attributes
;;                       "Unit Name" renderer "text" col-full-name)))
;;         (gtk-tree-view-append-column view column))

;;       (let* ((renderer (gtk-cell-renderer-text-new))
;;              (column (gtk-tree-view-column-new-with-attributes
;;                       "Type" renderer "text" col-type)))
;;         (gtk-tree-view-append-column view column))

;;       (let* ((renderer (gtk-cell-renderer-text-new))
;;              (column (gtk-tree-view-column-new-with-attributes
;;                       "Role" renderer "text" col-role)))
;;         (gtk-tree-view-append-column view column))

;;       (let* ((renderer (gtk-cell-renderer-text-new))
;;              (column (gtk-tree-view-column-new-with-attributes
;;                       "PV" renderer "text" col-pv)))
;;         (gtk-tree-view-append-column view column))

;;       (let* ((renderer (gtk-cell-renderer-text-new))
;;              (column (gtk-tree-view-column-new-with-attributes
;;                       "Move" renderer "text" col-mv)))
;;         (gtk-tree-view-append-column view column))

;;       (let* ((renderer (gtk-cell-renderer-text-new))
;;              (column (gtk-tree-view-column-new-with-attributes
;;                       "A/S" renderer "text" col-arm-struct)))
;;         (gtk-tree-view-append-column view column))

;;       (let* ((renderer (gtk-cell-renderer-text-new))
;;              (column (gtk-tree-view-column-new-with-attributes
;;                       "Attack" renderer "text" col-attack)))
;;         (gtk-tree-view-append-column view column))

;;       (let* ((renderer (gtk-cell-renderer-text-new))
;;              (column (gtk-tree-view-column-new-with-attributes
;;                       "OV" renderer "text" col-ov)))
;;         (gtk-tree-view-append-column view column))

;;       (let* ((renderer (gtk-cell-renderer-text-new))
;;              (column (gtk-tree-view-column-new-with-attributes
;;                       "Specials" renderer "text" col-specials)))
;;         (gtk-tree-view-append-column view column))
;;       view))

;;   (defun draw-unit-list ()
;;     (setf model (build-unit-list-model))
;;     (let ((layout (make-instance 'gtk-grid))
;;           (view (build-unit-list-view model))
;;           (title (make-instance 'gtk-label
;;                                :use-markup t
;;                                :label "<big>Unit List</big>")))
;;       (gtk-grid-attach layout title 0 0 1 1)
;;       (gtk-grid-attach layout view  0 1 1 1)
;;       layout)))
