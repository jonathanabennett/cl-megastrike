(in-package :megastrike)


(mito:connect-toplevel :sqlite3 :database-name ":memory:")

(mito:deftable mek ()
  ((short-name    :col-type (:varchar 16) :accessor mek/short-name)
   (long-name     :col-type (:varchar 64) :accessor mek/long-name)
   (unit-type     :col-type (:varchar 4)  :accessor mek/unit-type)
   (role          :col-type (:varchar 20) :accessor mek/role)
   (pv            :col-type :int          :accessor mek/pv)
   (size          :col-type :int          :accessor mek/size)
   (tro           :col-type :text         :accessor mek/tro)
   (armor         :col-type :int          :accessor mek/armor)
   (struct        :col-type :int          :accessor mek/struct)
   (mv-string     :col-type (:varchar 16) :accessor mek/mv-string)
   (short         :col-type :real         :accessor mek/short)
   (medium        :col-type :real         :accessor mek/medium)
   (long          :col-type :real         :accessor mek/long)
   (ov            :col-type :int          :accessor mek/ov)
   (display       :col-type :text         :accessor mek/display)
   (specials-str  :col-type :text         :accessor mek/specials)))

(mito:ensure-table-exists 'mek)

(defun add-or-update-mek (&rest rest &key short-name long-name unit-type role pv
                                       size (tro "") armor structure mv-string
                                       short medium long ov display specials)
  (if (mito:find-dao 'mek :short-name short-name)
      (apply #'update-mek-in-mul rest)
      (apply #'add-mek-to-mul rest)))

(defun add-mek-to-mul (&key short-name long-name unit-type role pv size (tro "")
                       armor structure mv-string short medium long ov display specials)
  (mito:create-dao 'mek :short-name short-name :long-name long-name :unit-type unit-type
                   :role role :pv pv :size size :tro tro :armor armor :struct structure
                   :mv-string mv-string :short short :medium medium :long long :ov ov
                   :display display :specials-str specials))

(defun update-mek-in-mul (&key short-name long-name unit-type role pv size (tro "")
                         armor structure mv-string short medium long ov display specials)
  (let ((m (mito:find-dao 'mek :short-name short-name)))
    (setf (slot-value m 'short-name)  short-name)
    (setf (slot-value m 'long-name)   long-name)
    (setf (slot-value m 'unit-type)   unit-type)
    (setf (slot-value m 'role)        role)
    (setf (slot-value m 'pv)          pv)
    (setf (slot-value m 'size)        size)
    (setf (slot-value m 'tro)         tro)
    (setf (slot-value m 'armor)       armor)
    (setf (slot-value m 'struct)      structure)
    (setf (slot-value m 'mv-string)   mv-string)
    (setf (slot-value m 'short)       short)
    (setf (slot-value m 'medium)      medium)
    (setf (slot-value m 'long)        long)
    (setf (slot-value m 'ov)          ov)
    (setf (slot-value m 'display)     display)
    (setf (slot-value m 'specials-str) specials)
    (mito:save-dao m)))

(define-presentation-method present (mek
                                     (type mek)
                                     stream
                                     (view textual-view) &key)
  (formatting-row (stream)
    (formatting-cell (stream) (write-string (mek/long-name mek) stream))
    (formatting-cell (stream) (format stream "~a" (mek/pv mek)))
    (formatting-cell (stream) (format stream "~a" (mek/size mek)))
    (formatting-cell (stream) (write-string (mek/mv-string mek) stream))
    (formatting-cell (stream) (write-string (display-attack-string mek) stream))
    (formatting-cell (stream) (format stream "~a" (mek/ov mek)))
    (formatting-cell (stream) (format stream "~d/~d" (mek/armor mek) (mek/struct mek)))
    (formatting-cell (stream) (write-string (mek/specials mek) stream))))

(defun display-attack-string (mek)
  (let ((short  (if (> (mek/short mek) 0.9)
                    (floor (mek/short mek))
                    (if (= (mek/short mek) 0) "0" "0*")))
        (medium (if (> (mek/medium mek) 0.9)
                    (floor (mek/medium mek))
                    (if (= (mek/medium mek) 0) "0" "0*")))
        (long   (if (> (mek/long mek) 0.9)
                    (floor (mek/long mek))
                    (if (= (mek/long mek) 0) "0" "0*"))))
    (format nil "~a/~a/~a" short medium long)))

(defun load-database ()
  (mito:retrieve-dao 'mek))
