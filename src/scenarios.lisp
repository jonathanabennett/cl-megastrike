;;;; scenarios.lisp
;;; Used to load scenario files.
;;; Scenario files use the same format as MegaMek to increase compatibility.

(defclass scenario ()
  ((name :initarg :name
         :initform "Example"
         :accessor scenario/name)
   (description :initarg :description
                :initform "An example description"
                :accessor scenario/description)
   (map-info :initarg :map-info
             :initform '()
             :accessor scenario/map-info
             :documentation "A nested list of maps")
   (conditions :initarg :conditions
               :initform nil
               :accessor scenario/conditions
               :documentat "The planetary conditions to use in the scenario.")
   (forces :initarg :forces
           :initform '()
           :accessor scenario/forces
           :documentation "The forces in the battle.")
   (units :initarg :units
          :initform '()
          :accessor scenario/units
          :documentation "The units in the battle")
   ))

;; Format for a unit line is
;; MechRef,PilotName,PilotGunnery,PilotPiloting,facing,x,y
;; Everything after PilotPiloting is optional
;; Example:
;; Unit_AFFS_1=Archer ARC-2R,Bob Smith,3,4,S,4,10
;; Unit_DCMS_1=HGN-732,Takashi,4,5
;; When converting to AlphaStrike, ignore PilotPiloting.
