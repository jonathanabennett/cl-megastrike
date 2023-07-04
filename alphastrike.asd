;;;; alphastrike.asd

(asdf:defsystem #:alphastrike
  :description "Describe alphastrike here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:random-uuid #:cl-json #:cl-ppcre #:mcclim)
  :components ((:file "package")
               (:file "hexagon")
               (:file "board")
               (:file "crew")
               (:file "element" :depends-on ("crew"))
               (:file "unitcard" :depends-on ("element"))
               (:file "alphastrike" :depends-on ("element" "unitcard" "board"))))
