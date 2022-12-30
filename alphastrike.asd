;;;; alphastrike.asd

(asdf:defsystem #:alphastrike
  :description "Describe alphastrike here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (:random-uuid #:mcclim)
  :components ((:file "package")
               (:file "crew")
               (:file "element" :depends-on ("crew"))
               (:file "alphastrike" :depends-on ("element"))))
