;;;; megastrike.lisp

(in-package #:megastrike)

(defparameter +color-list+ `(("Red" .    ,+red+)
                             ("Blue" .   ,+blue+)
                             ("Purple" . ,+purple+)
                             ("Green" .  ,+light-green+)
                             ("Gold" .   ,+gold+)))

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

(define-application-frame megastrike ()
  ((layout           :initarg :layout     :accessor frame/layout))
  (:menu-bar nil)
  (:panes
     (game-world
      :application :default-view +graphical-view+ :display-function #'display-map)
     (game-record-sheet
      :application :scroll-bars nil :min-width 375
                   :display-function #'display-record-sheet)
     (game-quickstats
      :application :scroll-bars nil :min-width 375
                   :default-view +quickstats-view+
                   :display-function #'display-quickstats)
     (lobby-overview
      :application :scroll-bars nil :display-function #'display-overview)
     (lobby-force-list
      :application :display-function #'display-lobby-force-list)
     (lobby-detail-view
      :application :min-width 375 :scroll-bars t
                   :display-function #'display-lobby-detail-view)
     ;; (int :interactor)
     (round-report
      :application :display-function #'display-round-report)
     (menu :command-menu)
     )
  (:layouts
   (:default
    (vertically ()
      (:fill
       (horizontally ()
         lobby-detail-view
         (vertically ()
           lobby-overview
           lobby-force-list)))
      ;;int
      (1/10 menu)
      ))
   (:game-round
    (vertically ()
      (:fill
       (horizontally ()
         (:fill game-world)
         (vertically ()
           game-record-sheet
           game-quickstats)))
      (1/10 menu)))
   (:round-report
    (vertically ()
      (:fill round-report)
      (1/10 menu)))))
