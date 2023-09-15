;;;; megastrike.asd

;; (ql:quickload :deploy)
;; (deploy:define-resource-directory bundle (uiop:merge-pathnames* "dists/" (uiop:getcwd)))

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
                 (:file "game")
                 (:file "stringlist")
                 (:file "mul")
                 (:file "lobby" :depends-on ("mul"))
                 (:file "hexagon")
                 (:file "tiles")
                 (:file "board" :depends-on ("hexagon" "tiles"))
                 (:file "combat-unit" :depends-on ("mul" "board"))
                 (:file "initiative" :depends-on ("utils"))
                 (:file "force" :depends-on ("combat-unit" "utils"))
                 (:file "unitcard" :depends-on ("combat-unit"))
                 (:file "megastrike" :depends-on ("utils" "unitcard"))
                 )))
  ;; :defsystem-depends-on (:deploy)
  ;; :build-operation "deploy-op"
  :build-pathname "megastrike"
  :entry-point "megastrike:main")
