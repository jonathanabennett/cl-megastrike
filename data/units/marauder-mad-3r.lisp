(in-package :alphastrike)

(defun marauder-mad-3r ()
  (new-element
   :short-name "MAD-3R"
   :full-name "Marauder MAD-3R"
   :unit-type 'BM
   :role 'sniper
   :pv 35
   :size 3
   :max-armor 6
   :max-struct 6
   :move-list (list
               (cons 'walk 4)
              )
   :short 2
   :medium 3
   :long 3 ;; Enter 0.5 for 0*
   :ov 1
   :cur-heat 0
   :special-list '('SRCH 'SOA)
   :crit-list '()
   :img #P"data/images/units/mechs/Marauder.png"
   :tro
""
))
