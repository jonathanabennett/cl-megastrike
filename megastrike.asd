;;;; alphastrike.asd

(asdf:defsystem #:megastrike
  :description "A port of the Alphastrike Board game to Computer using Common Lisp and McCLIM."
  :author "Jonathan A. Bennett <doulos05@gmail.com>"
  :license  "GPL3"
  :version "1.1.0"
  :serial t
  :depends-on (#:beast #:mcclim #:mito #:cl-ppcre #:str)
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "utils")
                 (:file "db")
                 (:file "hexagon")
                 (:file "tiles")
                 (:file "element" :depends-on ("utils"))
                 (:file "initiative" :depends-on ("utils"))
                 (:file "army" :depends-on ("element" "utils"))
                 (:file "unitcard" :depends-on ("element"))
                 (:file "board" :depends-on ("element" "hexagon" "tiles"))
                 (:file "game-setup" :depends-on ("element" "board" "army"))
                 (:file "megastrike" :depends-on ("element" "unitcard" "army" "board"))
                 (:file "commands" :depends-on ("megastrike"))))))
