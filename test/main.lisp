(defpackage :megastrike/test
  (:use :common-lisp :fiveam :megastrike))

(in-package :megastrike/test)

(def-suite* test-megastrike :description "Master test suite for Megastrike.")

(defun all-tests ()
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

(def-suite* test-hexagons :description "Testing the hexagon API."
                         :in test-megastrike)

(test hexagon-creation-tests
  (is (typep (megastrike::new-hexagon :q 0 :r 0 :s 0) 'megastrike::hexagon)
      "Failed to create a hexagon when given correct coordinates."))

(test hexagon-equality
  (let ((hex1 (megastrike::new-hexagon :q 0 :r 0 :s 0))
        (matching-hex (megastrike::new-hexagon :q 0 :r 0 :s 0))
        (mismatched-hex (megastrike::new-hexagon :q 0 :r 1 :s -1)))
    (is-true (megastrike::same-hex hex1 hex1)
             "A hex object should match itself.")
    (is-true (megastrike::same-hex hex1 matching-hex)
             "A hex object should match a hex that has the same q,r,s address.")
    (is-false (megastrike::same-hex hex1 mismatched-hex)
              "A hex object should NOT match a hex that has a different q,r,s address")
    (is-false (megastrike::same-hex "" hex1)
              "Disparate types are not the same.")
    (is-false (megastrike::same-hex hex1 "")
              "Disparate types are not the same.")))

(test hexagonal-addition
  (let ((base-hex (megastrike::new-hexagon :q 0 :r 0 :s 0))
        (diff-q-hex (megastrike::new-hexagon :q 1 :r 0 :s -1))
        (diff-r-hex (megastrike::new-hexagon :q 0 :r 1 :s -1))
        (diff-s-hex (megastrike::new-hexagon :q -1 :r -1 :s 2)))
    (is (megastrike::same-hex (megastrike::hex-addition base-hex diff-q-hex)
                              (megastrike::new-hexagon :q 1 :r 0 :s -1)))
    (is (megastrike::same-hex (megastrike::hex-addition base-hex diff-r-hex)
                              (megastrike::new-hexagon :q 0 :r 1 :s -1)))
    (is (megastrike::same-hex (megastrike::hex-addition base-hex diff-s-hex)
                              (megastrike::new-hexagon :q -1 :r -1 :s 2)))
    (is (megastrike::same-hex (megastrike::hex-addition diff-q-hex diff-r-hex)
                              (megastrike::new-hexagon :q 1 :r 1 :s -2)))))

(test hexagonal-subtraction
  (let ((base-hex (megastrike::new-hexagon :q 0 :r 0 :s 0))
        (diff-q-hex (megastrike::new-hexagon :q 1 :r 0 :s -1))
        (diff-r-hex (megastrike::new-hexagon :q 0 :r 1 :s -1))
        (diff-s-hex (megastrike::new-hexagon :q -1 :r -1 :s 2)))
    (is (megastrike::same-hex (megastrike::hex-subtract base-hex diff-q-hex)
                              (megastrike::new-hexagon :q -1 :r 0 :s 1)))
    (is (megastrike::same-hex (megastrike::hex-subtract base-hex diff-r-hex)
                              (megastrike::new-hexagon :q 0 :r -1 :s 1)))
    (is (megastrike::same-hex (megastrike::hex-subtract base-hex diff-s-hex)
                              (megastrike::new-hexagon :q 1 :r 1 :s -2)))
    (is (megastrike::same-hex (megastrike::hex-subtract diff-q-hex diff-r-hex)
                              (megastrike::new-hexagon :q 1 :r -1 :s -2)))))

(test hex-offset-converstion
  (let ((test-hex-00 (megastrike::new-hexagon :q 0 :r 0 :s 0))
        (test-offset-00 '(0 0))
        (test-hex-42 (megastrike::new-hexagon :q 4 :r 0 :s -4))
        (test-offset-42 '(4 2)))
    (is (megastrike::same-hex test-hex-00 (megastrike::hex-from-offset :col 0 :row 0)))
    (is (every #'eql test-offset-00 (megastrike::offset-from-hex test-hex-00)))
    (is (not (megastrike::same-hex test-hex-00 (megastrike::hex-from-offset :col 4 :row 2))))
    (is (not (every #'eql test-offset-00 (megastrike::offset-from-hex test-hex-42))))
    (is (megastrike::same-hex test-hex-42 (megastrike::hex-from-offset :col 4 :row 2))
        "This test failed.")
    (is (every #'eql test-offset-42 (megastrike::offset-from-hex test-hex-42)))
    (is (not (megastrike::same-hex test-hex-42 (megastrike::hex-from-offset :col 0 :row 0))))
    (is (not (every #'eql test-offset-42 (megastrike::offset-from-hex test-hex-00))))))
