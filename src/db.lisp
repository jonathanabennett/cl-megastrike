(in-package :megastrike)


(mito:connect-toplevel :sqlite3 :database-name ":memory:")

(defun load-database ()
  (mito:retrieve-dao 'mek))
