(defpackage :megastrike/test
  (:use :common-lisp :fiveam :megastrike))

(in-package :megastrike/test)

(def-suite* test-megastrike :description "Master test suite for Megastrike.")

(defun test-megastrike ()
  (run! 'test-megastrike))

(test mechset-parser-tests
  (let ((test-line "chassis \"Zechetinu II Corvette\" \"warships/zechetinu.png\"")
        (result '("chassis" "Zechetinu II Corvette" "warships/zechetinu.png")))
    (is (eq (megastrike::parse-mechset-line "# This is a comment") nil)
        "A commented mechset line should not be parsed.")
    (is (eq (megastrike::parse-mechset-line "") nil)
        "An empty line should not be parsed.")
    (is (eq (megastrike::parse-mechset-line "include localserver_mechset.txt") nil)
        "Include lines are not parsed yet.")
    (is (every #'string= (megastrike::parse-mechset-line test-line) result))))

(defun test-a-lot-of-rolls ()
  (every #'identity (loop for i from 1 to 1000000
                          collecting (let ((result (megastrike::roll 1)))
                                       (and (>= result 1)
                                            (<= result 6))))))
(test dice-rolling
  (is-true (test-a-lot-of-rolls)))
