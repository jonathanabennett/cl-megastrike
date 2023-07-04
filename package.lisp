;;;; package.lisp

(defpackage #:alphastrike
  (:use #:clim #:clim-lisp))

(defgeneric display (obj)
  (:documentation "Pretty print an object for display in the GUI"))
