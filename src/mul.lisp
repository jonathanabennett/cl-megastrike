(in-package :megastrike)

(defclass mek ()
  ((short-name   :initarg :short-name   :accessor mek/short-name)
   (long-name    :initarg :long-name    :accessor mek/long-name)
   (unit-type    :initarg :unit-type    :accessor mek/unit-type)
   (role         :initarg :role         :accessor mek/role)
   (pv           :initarg :pv           :accessor mek/pv)
   (size         :initarg :size         :accessor mek/size)
   (tro          :initarg :tro          :accessor mek/tro)
   (armor        :initarg :armor        :accessor mek/armor)
   (struct       :initarg :struct       :accessor mek/struct)
   (mv-string    :initarg :mv-string    :accessor mek/mv-string)
   (short        :initarg :short        :accessor mek/short)
   (medium       :initarg :medium       :accessor mek/medium)
   (long         :initarg :long         :accessor mek/long)
   (ov           :initarg :ov           :accessor mek/ov)
   (display      :initarg :display      :accessor mek/display)
   (specials-str :initarg :specials-str :accessor mek/specials)))

;; (defun mek-from-list-store (model iter)
;;   (let ((sn (gtk-tree-model-get-value model iter 0))
;;         (ln (gtk-tree-model-get-value model iter 1))
;;         (tp (gtk-tree-model-get-value model iter 2))
;;         (ro (gtk-tree-model-get-value model iter 3))
;;         (pv (gtk-tree-model-get-value model iter 4))
;;         (sz (gtk-tree-model-get-value model iter 5))
;;         (ar (gtk-tree-model-get-value model iter 6))
;;         (st (gtk-tree-model-get-value model iter 7))
;;         (mv (gtk-tree-model-get-value model iter 8))
;;         (sh (gtk-tree-model-get-value model iter 9))
;;         (me (gtk-tree-model-get-value model iter 10))
;;         (lo (gtk-tree-model-get-value model iter 11))
;;         (ov (gtk-tree-model-get-value model iter 12))
;;         (sp (gtk-tree-model-get-value model iter 13))
;;         (di (gtk-tree-model-get-value model iter 14)))
;;     (make-instance 'mek
;;                    :short-name sn :long-name ln :unit-type tp :role ro :pv pv
;;                    :size sz :armor ar :struct st :mv-string mv :short sh :medium me
;;                    :long lo :ov ov :display di :specials-str sp :tro nil)))


;; (let ((col-mek-short-name 0) (col-mek-long-name 1) (col-mek-type 2) (col-mek-role 3)
;;       (col-mek-pv 4) (col-mek-size 5) (col-mek-armor 6) (col-mek-struct 7) (col-mek-mv 8)
;;       (col-mek-short 9) (col-mek-medium 10) (col-mek-long 11) (col-mek-ov 12)
;;       (col-mek-specials 13) (col-mek-display 14) (model nil) (view nil) (appender nil))

;;   (defun new-mek (&key short-name long-name unit-type role pv size (tro "")
;;                     armor structure mv-string short medium long ov display specials)
;;     (add-unit-to-store
;;      model appender
;;      (make-instance 'mek :short-name short-name :long-name long-name :unit-type unit-type
;;                          :role role :pv pv :size size :tro tro :armor armor
;;                          :struct structure :mv-string mv-string :short short
;;                          :medium medium :long long :ov ov :display display
;;                          :specials-str specials)))

;;   (defun add-unit-to-store (model iter mek)
;;     (gtk-list-store-set model iter
;;                         (mek/short-name mek) (mek/long-name mek)
;;                         (mek/unit-type mek) (mek/role mek) (mek/pv mek)
;;                         (mek/size mek) (mek/armor mek) (mek/struct mek)
;;                         (mek/mv-string mek) (mek/short mek) (mek/medium mek)
;;                         (mek/long mek) (mek/ov mek) (mek/specials mek)
;;                         (mek/display mek)))

;;   (defun build-unit-model ()
;;     (let ((mech-files (uiop:directory-files (uiop:merge-pathnames* #p"data/units/" *here*))))
;;       (setf model (make-instance 'gtk-list-store
;;                          :column-types '("gchararray" "gchararray" "gchararray" "gchararray"
;;                                          "gint" "gint" "gint" "gint" "gchararray"
;;                                          "gint" "gint" "gint" "gint" "gchararray"
;;                                          "gchararray")))
;;       (dolist (file mech-files)
;;         (if (string= (pathname-type file) "lisp")
;;             (progn
;;               (setf appender (gtk-list-store-append model))
;;               (load file))))
;;       model))

;;   (defun build-unit-view ()
;;     (let ((view (gtk-tree-view-new-with-model model)))
;;       (let* ((renderer (gtk-cell-renderer-text-new))
;;              (column (gtk-tree-view-column-new-with-attributes
;;                       "Short name" renderer "text" col-mek-short-name)))
;;         (gtk-tree-view-append-column view column))

;;       (let* ((renderer (gtk-cell-renderer-text-new))
;;              (column (gtk-tree-view-column-new-with-attributes
;;                       "Full name" renderer "text" col-mek-long-name)))
;;         (gtk-tree-view-append-column view column))

;;       (let* ((renderer (gtk-cell-renderer-text-new))
;;              (column (gtk-tree-view-column-new-with-attributes
;;                       "Type" renderer "text" col-mek-type)))
;;         (gtk-tree-view-append-column view column))

;;       (let* ((renderer (gtk-cell-renderer-text-new))
;;              (column (gtk-tree-view-column-new-with-attributes
;;                       "Role" renderer "text" col-mek-role)))
;;         (gtk-tree-view-append-column view column))

;;       (let* ((renderer (gtk-cell-renderer-text-new))
;;              (column (gtk-tree-view-column-new-with-attributes
;;                       "PV" renderer "text" col-mek-pv)))
;;         (gtk-tree-view-append-column view column))

;;       (let* ((renderer (gtk-cell-renderer-text-new))
;;              (column (gtk-tree-view-column-new-with-attributes
;;                       "Size" renderer "text" col-mek-size)))
;;         (gtk-tree-view-append-column view column))

;;       (let* ((renderer (gtk-cell-renderer-text-new))
;;              (column (gtk-tree-view-column-new-with-attributes
;;                       "Armor" renderer "text" col-mek-armor)))
;;         (gtk-tree-view-append-column view column))

;;       (let* ((renderer (gtk-cell-renderer-text-new))
;;              (column (gtk-tree-view-column-new-with-attributes
;;                       "Structure" renderer "text" col-mek-struct)))
;;         (gtk-tree-view-append-column view column))

;;       (let* ((renderer (gtk-cell-renderer-text-new))
;;              (column (gtk-tree-view-column-new-with-attributes
;;                       "Move" renderer "text" col-mek-mv)))
;;         (gtk-tree-view-append-column view column))

;;       (let* ((renderer (gtk-cell-renderer-text-new))
;;              (column (gtk-tree-view-column-new-with-attributes
;;                       "S" renderer "text" col-mek-short)))
;;         (gtk-tree-view-append-column view column))

;;       (let* ((renderer (gtk-cell-renderer-text-new))
;;              (column (gtk-tree-view-column-new-with-attributes
;;                       "M" renderer "text" col-mek-medium)))
;;         (gtk-tree-view-append-column view column))

;;       (let* ((renderer (gtk-cell-renderer-text-new))
;;              (column (gtk-tree-view-column-new-with-attributes
;;                       "L" renderer "text" col-mek-long)))
;;         (gtk-tree-view-append-column view column))

;;       (let* ((renderer (gtk-cell-renderer-text-new))
;;              (column (gtk-tree-view-column-new-with-attributes
;;                       "OV" renderer "text" col-mek-ov)))
;;         (gtk-tree-view-append-column view column))

;;       (let* ((renderer (gtk-cell-renderer-text-new))
;;              (column (gtk-tree-view-column-new-with-attributes
;;                       "Specials" renderer "text" col-mek-specials)))
;;         (gtk-tree-view-append-column view column))

;;       ;; (let* ((renderer (gtk-cell-renderer-text-new))
;;       ;;        (column (gtk-tree-view-column-new-with-attributes
;;       ;;                 "Image" renderer "text" col-mek-display)))
;;       ;;   (gtk-tree-view-append-column view column))
;;       view))

;;   (defun draw-unit-selection ()
;;     (let ((layout (make-instance 'gtk-grid))
;;           (title (make-instance 'gtk-label
;;                                 :use-markup t
;;                                 :label "<big>Unit Selection</big>"))
;;           (new-unit-box (make-instance 'gtk-box
;;                                        :orientation :horizontal))
;;           (pname-label (make-instance 'gtk-label
;;                                       :use-markup t
;;                                       :label "<b>Pilot Name: </b>"))
;;           (pname-entry (make-instance 'gtk-entry
;;                                       :width-chars 20))
;;           (pskill-label (make-instance 'gtk-label
;;                                       :use-markup t
;;                                       :label "<b>Pilot Skill: </b>"))
;;           (pskill-entry (make-instance 'gtk-entry
;;                                       :use-markup t
;;                                       :width-chars 5))
;;           (new-unit-button (gtk-button-new-with-label "Add unit to force"))
;;           )
;;       (setf model (build-unit-model))
;;       (setf view (build-unit-view))
;;       (let ((selection (gtk-tree-view-get-selection view)))
;;         (setf (gtk-tree-selection-mode selection) :single)
;;         (g-signal-connect selection "changed"
;;                           (lambda (object)
;;                             (let* ((view (gtk-tree-selection-get-tree-view object))
;;                                    (model (gtk-tree-view-model view))
;;                                    (iter (gtk-tree-selection-get-selected object)))
;;                               (setf (lobby/selected-mek *lobby*)
;;                                     (mek-from-list-store model iter))))))
;;       (g-signal-connect new-unit-button "clicked"
;;                         (lambda (widget)
;;                           (declare (ignore widget))
;;                           (let ((name (gtk-entry-text pname-entry))
;;                                 (skill (gtk-entry-text pskill-entry)))
;;                             (if (and (game/selected-force *game*)
;;                                      (lobby/selected-mek *lobby*)
;;                                      name
;;                                      (parse-integer skill))
;;                                 (let ((cu (new-element-from-mul
;;                                            (lobby/selected-mek *lobby*)
;;                                            :pname name :pskill skill)))
;;                                   (add-unit (game/selected-force *game*)
;;                                             cu)
;;                                   (add-to-unit-list cu)
;;                                   (update-forces))))))
;;       (gtk-box-pack-start new-unit-box pname-label)
;;       (gtk-box-pack-start new-unit-box pname-entry)
;;       (gtk-box-pack-start new-unit-box pskill-label)
;;       (gtk-box-pack-start new-unit-box pskill-entry)
;;       (gtk-box-pack-start new-unit-box new-unit-button)
;;       (gtk-grid-attach layout view 0 1 1 1)
;;       (gtk-grid-attach layout title 0 0 1 1)
;;       (gtk-grid-attach layout new-unit-box 0 2 1 1)
;;       layout)))
