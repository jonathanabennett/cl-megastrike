(in-package :alphastrike)

(defun phoenix-hawk-pxh-1d ()
  (new-element
   :short-name "PXH-1D"
   :full-name "Phoenix Hawk PXH-1D"
   :unit-type 'BM
   :role 'skirmisher
   :pv 26
   :size 2
   :max-armor 4
   :max-struct 4
   :move-list (list
               (cons 'walk 6)
               (cons 'jump 6)
              )
   :short 2
   :medium 2
   :long 0 ;; Enter 0.5 for 0*
   :ov 0
   :cur-heat 0
   :special-list '('SRCH 'SOA 'ENE)
   :crit-list '()
   :img #P"data/images/units/mechs/Phoenixhawk_1D.png"
   :tro
""
))
