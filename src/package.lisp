;;;; package.lisp

(defpackage #:megastrike
  (:use #:gtk #:gdk #:gdk-pixbuf #:gobject #:glib #:gio #:pango #:cairo
        #:cffi #:common-lisp #:beast)
  (:export :main))
