(in-package :megastrike)


(defclass game ()
  ((active-unit      :initform nil                   :accessor game/active-unit)
   (forces           :initarg :forces                :accessor game/forces
                     :initform '())
   (selected-force   :initform nil                   :accessor game/selected-force)
   (game-board       :initform (make-instance 'grid) :accessor game/board
                     :initarg :game-board)
   (current-phase    :initform 0                     :accessor game/current-phase
                     :initarg :current-phase)
   (turn-number      :initform 0                     :accessor game/turn-number
                     :initarg :turn-number)
   (phase-log        :initform ""                    :accessor game/phase-log
                     :initarg :phase-log)
   (initiative-list  :initform '()                   :accessor game/initiative-list
                     :initarg :initiative-list)
   (initiative-place :initform 0                     :accessor game/initiative-place
                     :initarg :initiative-place)))

(defun new-game ()
  (make-instance 'game))

(defclass lobby ()
  ((selected-mek  :initform nil :accessor lobby/selected-mek)
   (game          :initform nil :accessor lobby/game
                  :initarg :game)))


(defun new-lobby ()
  (let ((g (new-game)))
    (make-instance 'lobby :game g)))
