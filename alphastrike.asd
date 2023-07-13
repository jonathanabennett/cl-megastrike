;;;; alphastrike.asd

(asdf:defsystem #:alphastrike
  :description "Describe alphastrike here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:random-uuid #:cl-json #:beast #:cl-ppcre #:mcclim)
  :components ((:file "package")
               (:file "utils")
               (:file "random")
               (:file "hexagon")
               (:file "tiles")
               (:file "element" :depends-on ("random"))
               (:file "unitcard" :depends-on ("element"))
               (:file "board" :depends-on ("element" "hexagon" "tiles"))
               (:file "alphastrike" :depends-on ("element" "unitcard" "board"))))
