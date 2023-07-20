(in-package :megastrike)

(defparameter *db* (merge-pathnames #P"data/units/units.db" *here*))



(mito:connect-toplevel :sqlite3 :database-name *db*)
