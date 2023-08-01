;;;; megastrike.asd

;; (ql:quickload :deploy)
;; (deploy:define-resource-directory bundle (uiop:merge-pathnames* "dists/" (uiop:getcwd)))

(asdf:defsystem #:megastrike
  :description "A port of the Alphastrike Board game to Computer using Common Lisp and QTools."
  :author "Jonathan A. Bennett <doulos05@gmail.com>"
  :license  "GPL3"
  :version "1.2.0"
  :serial t
  :depends-on (:beast :cl-cffi-gtk :mito :cl-ppcre :str :trivial-backtrace)
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "utils")
                 (:file "game")
                 (:file "db")
                 (:file "hexagon")
                 (:file "tiles")
                 (:file "element" :depends-on ("utils"))
                 (:file "initiative" :depends-on ("utils"))
                 (:file "force" :depends-on ("element" "utils"))
                 (:file "unitcard" :depends-on ("element"))
                 (:file "board" :depends-on ("element" "hexagon" "tiles"))
                 (:file "game-setup" :depends-on ("element" "board" "force"))
                 ;; (:file "systems" :depends-on ("element" "force"))
                 (:file "megastrike" :depends-on ("utils" ))
                 )))
  :defsystem-depends-on (:deploy)
  :build-operation "deploy-op"
  :build-pathname "megastrike"
  :entry-point "megastrike:main")
