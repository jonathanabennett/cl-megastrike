(in-package :megastrike)

(defun longbow-lgb-0w ()
  (new-element
   :short-name "LGB-0W"
   :full-name "Longbow LGB-0W"
   :unit-type 'BM
   :role 'missile-boat
   :pv 36
   :size 4
   :max-armor 5
   :max-struct 7
   :move-list (list
               (cons 'walk 4)
              )
   :short 2
   :medium 3
   :long 3 ;; Enter 0.5 for 0*
   :ov 0
   :special-list '(SRCH SOA IF3 LRM1/3/3/)
   :img #P"data/images/units/mechs/Longbow.png"
   :tro
""
))
