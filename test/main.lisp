(defpackage :megastrike/test
  (:use :common-lisp :fiveam :megastrike))

(in-package :megastrike/test)

(def-suite* megastrike)

(test initial-test
  (is (= 1 1)))
