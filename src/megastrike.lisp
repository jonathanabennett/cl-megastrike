;;;; megastrike.lisp

(in-package #:megastrike)

(defvar *test-mek*)
(defvar *game* (new-game))

(defun main ()
  (mito:connect-toplevel :sqlite3 :database-name ":memory:")
  (mito:ensure-table-exists 'mek)
  (load-data)
  (build-mul)
  (setf (game/active-unit *game*) (new-element-from-mul (mito:find-dao 'mek :id 1)
                                                      :pname "Bob" :pskill 4))
  (setf (game/board *game*) (make-grid 16 17))
  (setf (heat/cur-heat (game/active-unit *game*)) 2)
  (setf (damageable/cur-armor (game/active-unit *game*)) 2)
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Megastrike"
                                 :width 1000))
          (game-layout (make-instance 'gtk-grid
                                      :spacing 5))
          (map-area (make-instance 'gtk-drawing-area
                                   ))
          (recordsheet (draw-stats)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (g-signal-connect map-area "draw"
        (lambda (widget cr)
          (let ((cr (pointer cr))
                ;; Get the GdkWindow for the widget
                (window (gtk-widget-window widget)))
          ;; Clear surface
          (cairo-set-source-rgb cr 1.0 1.0 1.0)
          (cairo-paint cr)
          (cairo-scale cr
                       (gdk-window-get-width window)
                       (gdk-window-get-height window))
          ;; Example is in 1.0 x 1.0 coordinate space
          ;; Drawing code goes here
          (cairo-set-line-width cr 0.1)
          (cairo-set-source-rgb cr 1.0 0.0 0.0)
          (cairo-rectangle cr 0.25 0.25 0.5 0.5)
          (cairo-stroke cr)
          ;; Destroy the Cario context
          (cairo-destroy cr)
          t)))
      (gtk-grid-attach game-layout map-area 0 0 2 2)
      (gtk-grid-attach-next-to game-layout recordsheet map-area :right 1 1)
      (gtk-container-add window game-layout)
      (gtk-widget-show-all window))))

(defun draw-stats ()
  (let ((grid (make-instance 'gtk-grid
                               :spacing 10))
        (unit-id-line (make-instance 'gtk-label
                                     :use-markup t
                                     :halign :start
                                     :label (format nil "<big>~a</big>~4@t ID: ~a"
                                                   (info/full-name (game/active-unit *game*))
                                                   (entity-id (game/active-unit *game*)))))
        (info-line (general-info-block (game/active-unit *game*)))
        (attack-line (attack-info-block (game/active-unit *game*)))
        (heat-line (heat-info-block (game/active-unit *game*)))
        (armor-levels (damage-info-block (game/active-unit *game*)))
        (specials (specials-info-block (game/active-unit *game*)))
        )
    (gtk-grid-attach grid unit-id-line 0 0 1 1)
    (gtk-grid-attach grid info-line 0 1 1 1)
    (gtk-grid-attach grid attack-line 0 2 1 1)
    (gtk-grid-attach grid heat-line 0 3 1 1)
    (gtk-grid-attach grid armor-levels 0 4 1 1)
    (gtk-grid-attach grid specials 0 5 1 1)
    grid))
