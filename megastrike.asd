;;;; megastrike.asd

(asdf:defsystem #:megastrike
  :description "A port of the Alphastrike Board game to Computer using Common Lisp and QTools."
  :author "Jonathan A. Bennett <doulos05@gmail.com>"
  :license  "GPL3"
  :version "1.2.0"
  :serial t
  :depends-on (:cl-gtk4 :cl-gdk4 :cl-cairo2 :fuzzy-match :cl-slug :cl-csv :cl-ppcre :uuid :str)
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "utils")
                 (:file "stringlist")
                 (:file "hexagon")
                 (:file "tiles")
                 (:file "mul")
                 (:file "lobby" :depends-on ("mul"))
                 (:file "board" :depends-on ("hexagon" "tiles"))
                 (:file "combat-unit" :depends-on ("mul" "board"))
                 (:file "initiative" :depends-on ("utils"))
                 (:file "deployment")
                 (:file "movement")
                 (:file "combat")
                 (:file "end")
                 (:file "phases" :depends-on ("initiative"))
                 (:file "force" :depends-on ("combat-unit" "utils"))
                 (:file "game" :depends-on ("board" "force"))
                 (:file "unitcard" :depends-on ("combat-unit"))
                 (:file "megastrike" :depends-on ("utils" "unitcard" "game"))
                 )))
  :build-operation "program-op"
  :in-order-to ((test-op (test-op :megastrike/test)))
  :build-pathname "megastrike"
  :entry-point "megastrike:megastrike")

(asdf:defsystem #:megastrike/test
  :description "Test suite for Megastrike."
  :author "Jonathan A. Bennett"
  :license "GPL3"
  :depends-on (:fiveam :megastrike)
  :components ((:module "test"
                :components
                ((:file "main"))))
  :perform (test-op (op c) (symbol-call :fiveam :run! (find-symbol* :megastrike :megastrike/test))))
