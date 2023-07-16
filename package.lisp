;;;; package.lisp

(defpackage #:megastrike
  (:use #:clim #:clim-lisp #:beast)
  )

(defgeneric display (obj)
  (:documentation "Pretty print an object for display in the GUI"))
