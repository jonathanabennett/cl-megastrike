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
  ((chassis   :initarg :chassis   :accessor mek/chassis)
   (model     :initarg :model     :accessor mek/model)
   (role      :initarg :role      :accessor mek/role)
   (unit-type :initarg :type      :accessor mek/type)
   (size      :initarg :size      :accessor mek/size)
   (movement  :initarg :movement  :accessor mek/movement)
   (tmm       :initarg :tmm       :accessor mek/tmm
              :documentation "TMM from the distance only of the first movement type. Does not include movement type modifier like jumping")
   (armor     :initarg :armor     :accessor mek/armor)
   (structure :initarg :structure :accessor mek/structure)
   (threshold :initarg :threshold :accessor mek/threshold)
   (short     :initarg :short     :accessor mek/short)
   (short*    :initarg :short*    :accessor mek/short*)
   (medium    :initarg :medium    :accessor mek/medium)
   (medium*   :initarg :medium*   :accessor mek/medium*)
   (long      :initarg :long      :accessor mek/long)
   (long*     :initarg :long*     :accessor mek/long*)
   (extreme   :initarg :extreme   :accessor mek/extreme)
   (extreme*  :initarg :extreme*  :accessor mek/extreme*)
   (ov        :initarg :ov        :accessor mek/ov)
   (pv        :initarg :pv        :accessor mek/pv)
   (display   :initarg :display   :accessor mek/display)
   (abilities :initarg :abilities :accessor mek/abilities)
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
  (let ((uuid (uuid:make-v1-uuid))
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
    (if (string= (cdr (assoc 'type data)) "BM")
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
                       (cdr (assoc 'ov data)) (cdr (assoc 'pv data))
                       (cdr (assoc 'abilities data)) (cdr (assoc 'front-arc data))
                       (cdr (assoc 'left-arc data)) (cdr (assoc 'right-arc data))
                       (cdr (assoc 'rear-arc data)))))))

(defun construct-mv-alist (mv-string)
  (let ((mv-alist '())
         (mv-strings (ppcre:all-matches-as-strings
                      (ppcre:create-scanner "\\d+\\\"[a-zA-z]?") mv-string)))
    (dolist (str mv-strings)
      (multiple-value-bind (dist type) (extract-numbers-and-letters str)
        (setf mv-alist (acons (rassoc type *mv-designators* :test #'string=)
                              (/ (parse-integer dist) 2) mv-alist))))
    mv-alist))

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

(defun draw-mul ()
  (let (name skill)
    (let* ((layout (gtk:make-grid))
           (mul (mul-list-view))
           )
      )))

(defun mul-list-view ()
  (let* ((model (gtk:make-string-list :strings (loop for uuid being the hash-keys of *mul*
                                                     :collect uuid)))
         (view (gtk-make-column-view :model (gtk-make-single-selection :model model)))
         (chassis-factory (gtk:make-signal-list-item-factory))
         (chassis-col (gtk:make-column-view-column :title "Chassis" :factory chassis-factory))
         (role-factory (gtk:make-signal-list-item-factory))
         (role-col (gtk:make-column-view-column :title "Role" :factory role-factory))
         (type-factory (gtk:make-signal-list-item-factory))
         (type-col (gtk:make-column-view-column :title "Type" :factory type-factory))
         (size-factory (gtk:make-signal-list-item-factory))
         (size-col (gtk:make-column-view-column :title "Size" :factory size-factory))
         (movement-factory (gtk:make-signal-list-item-factory))
         (movement-col (gtk:make-column-view-column :title "Movement" :factory movement-factory))
         (tmm-factory (gtk:make-signal-list-item-factory))
         (tmm-col (gtk:make-column-view-column :title "TMM" :factory tmm-factory))
         (as-factory (gtk:make-signal-list-item-factory))
         (as-col (gtk:make-column-view-column :title "Arm/Struct" :factory as-factory))
         (attacks-factory (gtk:make-signal-list-item-factory))
         (attacks-col (gtk:make-column-view-column :title "S/M/L/E" :factory attacks-factory))
         (tmm-factory (gtk:make-signal-list-item-factory))
         (tmm-col (gtk:make-column-view-column :title "TMM" :factory tmm-factory))
         (tmm-factory (gtk:make-signal-list-item-factory))
         (tmm-col (gtk:make-column-view-column :title "TMM" :factory tmm-factory))
         (tmm-factory (gtk:make-signal-list-item-factory))
         (tmm-col (gtk:make-column-view-column :title "TMM" :factory tmm-factory))
         (tmm-factory (gtk:make-signal-list-item-factory))
         (tmm-col (gtk:make-column-view-column :title "TMM" :factory tmm-factory))
         )))
