(in-package :megastrike)


(defclass game ()
  ((units            :initarg :units      :accessor game/units
                     :initform '())
   (active-unit      :initform nil        :accessor game/active-unit)
   (forces           :initarg :forces     :accessor game/forces-hash
                     :initform (make-hash-table :test #'equal))
   (selected-force   :initform nil        :accessor game/selected-force)
   (game-board       :initarg :game-board :accessor game/board
                     :initform nil)
   (current-phase    :initform -1         :accessor game/current-phase
                     :initarg :current-phase)
   (turn-number      :initform 0          :accessor game/turn-number
                     :initarg :turn-number)
   (phase-log        :initform ""         :accessor game/phase-log :initarg :phase-log)
   (initiative-list  :initform '()        :accessor game/initiative-list
                     :initarg :initiative-list)
   (initiative-place :initform 0          :accessor game/initiative-place
                     :initarg :initiative-place)))

(defun new-game ()
  (make-instance 'game))

;; Define Methods for accessing forces from the game using the hashtable we've created.
;; The hashtable is set up so that the key is `uuid' and the value is `force'
;; `game/forces' MUST return the list of values

(defmethod game/forces ((g game))
  (loop for value being the hash-values of (game/forces-hash g)
        :collect value))

(defmethod game/find-force ((g game) u)
  (gethash u (game/forces-hash g)))

(defmethod game/tile-occupied-p ((g game) (h hexagon))
  (dolist (u (game/units *game*))
    (when (same-hex h (cu/location u))
        (return t)))
  nil)

(defmethod print-initiative ((g game))
  (format nil "Initiative Order: ~{~a~^, ~}" (subseq (game/initiative-list g) (game/initiative-place g))))
