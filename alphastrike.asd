;;;; alphastrike.asd

(asdf:defsystem #:alphastrike
  :description "A port of the Alphastrike Board game to Computer using Common Lisp and McCLIM."
  :author "Jonathan A. Bennett <doulos05@gmail.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:beast #:mcclim)
  :components ((:file "package")
               (:file "utils")
               (:file "random")
               (:file "hexagon")
               (:file "tiles")
               (:file "element" :depends-on ("random"))
               (:file "unitcard" :depends-on ("element"))
               (:file "board" :depends-on ("element" "hexagon" "tiles"))
               (:file "alphastrike" :depends-on ("element" "unitcard" "board"))))
