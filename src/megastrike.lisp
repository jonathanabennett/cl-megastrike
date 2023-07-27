;;;; megastrike.lisp

(in-package #:megastrike)

(defparameter +color-list+ `(("Red" .    ,+red+)
                             ("Blue" .   ,+blue+)
                             ("Purple" . ,+purple+)
                             ("Green" .  ,+light-green+)
                             ("Gold" .   ,+gold+)))

(define-application-frame megastrike ()
  ((active-unit      :initform nil        :accessor active-unit)
   (armies           :initarg :armies     :accessor frame/armies
                         :initform '())
   (selected-army    :initform nil        :accessor lobby/selected-army)
   (selected-mek     :initform nil        :accessor lobby/selected-mek)
   (detail-unit      :initform nil        :accessor lobby/detail-unit)
   (game-board       :initarg :game-board :initform (make-instance 'grid)
                         :accessor frame/game-board)
   (current-phase    :initform 0          :accessor current-phase)
   (phase-log        :initform ""         :accessor phase-log)
   (turn-number      :initform 0          :accessor turn-number)
   (initiative-list  :initform '()        :accessor initiative-list)
   (initiative-place :initform 0          :accessor initiative-place)
   (layout           :initarg :layout     :accessor frame/layout))
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
     (lobby-army-list
      :application :display-function #'display-lobby-army-list)
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
           lobby-army-list)))
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
