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
   (unit-type :initarg :type      :initform nil :accessor mek/type)
   (role      :initarg :role      :initform nil :accessor mek/role)
   (pv        :initarg :pv        :initform 0   :accessor mek/pv)
   (size      :initarg :size      :initform 0   :accessor mek/size)
   (movement  :initarg :movement  :initform '()  :accessor mek/movement)
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
   (abilities :initarg :abilities :initform ""  :accessor mek/abilities)
   (display   :initarg :display   :initform nil :accessor mek/display)
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
  (let ((data (loop for key in header-row
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
    (let* ((full-name (format nil "~a ~a" (cdr (assoc 'chassis data)) (cdr (assoc 'model data)))))
      (setf (gethash full-name *mul*)
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
  (mek/comparable-num #'mek/ov m))

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
        (setf mv-alist (acons (car (rassoc type *mv-designators* :test #'string=))
                              (/ (parse-integer dist) 2) mv-alist))))
    (reverse mv-alist)))

(defun format-move-assoc (stream m colonp atsignp)
  "The colonp and atsignp are required for a function called inside `FORMAT'."
  (declare (ignorable colonp atsignp))
  (format stream "~a~a" (cdr m) (cdr (assoc (car m) *mv-designators*))))

(defmethod print-movement ((m mek))
  (if m
      (format nil "~{~/megastrike::format-move-assoc/~^/~}" (mek/movement m))
      "None"))

(defun filter-mek (filter m)
  (if (and filter m)
      (let ((filt-name (or (mek/chassis filter) nil))
            (filt-type (or (mek/type filter) nil)))
        (and (if filt-name (search filt-name (mek/full-name m)) t)
             (if filt-type (member (mek/type m) filt-type :test #'string=) t)))
      t))

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
