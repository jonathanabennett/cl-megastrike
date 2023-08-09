;;;; megastrike.asd

;; (ql:quickload :deploy)
;; (deploy:define-resource-directory bundle (uiop:merge-pathnames* "dists/" (uiop:getcwd)))

(asdf:defsystem #:megastrike
  :description "A port of the Alphastrike Board game to Computer using Common Lisp and QTools."
  :author "Jonathan A. Bennett <doulos05@gmail.com>"
  :license  "GPL3"
  :version "1.2.0"
  :serial t
  :depends-on (:beast :cl-gtk4 :cl-ppcre :str)
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "utils")
                 (:file "game")
                 (:file "mul")
                 (:file "lobby" :depends-on ("mul"))
                 ;; (:file "hexagon")
                 ;; (:file "tiles")
                 ;; (:file "board" :depends-on ("hexagon" "tiles"))
                 ;; (:file "element" :depends-on ("utils" "board"))
                 ;; (:file "initiative" :depends-on ("utils"))
                 ;; (:file "force" :depends-on ("element" "utils"))
                 ;; (:file "unitcard" :depends-on ("element"))
                 ;; (:file "game-setup" :depends-on ("element" "board" "force"))
                 ;; (:file "systems" :depends-on ("element" "force"))
                 (:file "megastrike" :depends-on ("utils" ))
                 )))
  ;; :defsystem-depends-on (:deploy)
  ;; :build-operation "deploy-op"
  :build-pathname "megastrike"
  :entry-point "megastrike:main")
