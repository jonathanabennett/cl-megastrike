;;;; megastrike.lisp

(in-package #:megastrike)

(defvar *test-mek*)
(defvar *game* (new-game))
(defvar *current-layout* :lobby)

(defun main ()
  (mito:connect-toplevel :sqlite3 :database-name ":memory:")
  (mito:ensure-table-exists 'mek)
  (load-data)
  (build-mul)
  (setf (game/active-unit *game*) (new-element-from-mul (mito:find-dao 'mek :id 1)
                                                      :pname "Bob" :pskill 4))
  (setf (heat/cur-heat (game/active-unit *game*)) 2)
  (setf (damageable/cur-armor (game/active-unit *game*)) 2)
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Megastrike"
                                 :width 1000))
          (game-view nil)
          (lobby-view nil)
          (layout (make-instance 'gtk-grid
                                      :row-homogeneous nil
                                      :vexpand nil))
          (command-menu (make-instance 'gtk-button-box
                                       :orientation :horizontal
                                       :height-request 30
                                       :layout-style :spread))
          (layout-button (gtk-button-new-with-label "Launch Game")))
      (g-signal-connect layout-button "clicked"
                        (lambda (widget)
                          (declare (ignore widget))
                          (if (gtk-widget-is-visible lobby-view)
                              (progn
                                (gtk-container-remove layout lobby-view)
                                (gtk-grid-attach layout game-view 0 0 1 1)))
                          (gtk-widget-show-all window)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))

      (setf game-view (draw-gameplay-screen))
      (setf lobby-view (draw-lobby-screen))
      (gtk-container-add command-menu layout-button)
      (gtk-grid-attach layout lobby-view 0 0 1 1)
      (gtk-grid-attach layout command-menu 0 1 1 1)
      (gtk-container-add window layout)
      (gtk-widget-show-all window))))

(defun draw-lobby-screen ()
  (let ((layout (make-instance 'gtk-grid
                               :name "lobby"
                               :hexpand t
                               :vexpand t
                               :spacing 5))
        (map-selection (draw-map-selection))
        (force-setup (draw-force-setup))
        (unit-selection (draw-unit-selection))
        (unit-list (draw-unit-list)))
    (gtk-grid-attach layout map-selection  0 0 1 1)
    (gtk-grid-attach layout unit-selection 1 0 1 1)
    (gtk-grid-attach layout force-setup    0 1 1 1)
    (gtk-grid-attach layout unit-list      1 1 1 1)
    layout))

(defun draw-map-selection ()
  (let ((layout (make-instance 'gtk-grid))
        (title (make-instance 'gtk-label
                              :use-markup t
                              :label "<big>Map Selection</big>"))
        (width-label (make-instance 'gtk-label
                                    :use-markup t
                                    :label "<b>Map Width: </b>"))
        (width-entry (make-instance 'gtk-entry
                                     :width-chars 10))
        (height-label (make-instance 'gtk-label
                                    :use-markup t
                                    :label "<b>Map Width: </b>"))
        (height-entry (make-instance 'gtk-entry
                                     :width-chars 10))
        (create-button (gtk-button-new-with-label "Create Map")))
    (g-signal-connect create-button "clicked"
                      (lambda (widget)
                        (declare (ignore widget))
                        (let ((w (parse-integer (gtk-entry-text width-entry)))
                              (h (parse-integer (gtk-entry-text height-entry))))
                          (setf (game/board *game*) (make-grid w h)))))
    (gtk-grid-attach layout title 0 0 2 1)
    (gtk-grid-attach layout width-label 0 1 1 1)
    (gtk-grid-attach-next-to layout width-entry width-label :right 1 1)
    (gtk-grid-attach layout height-label 0 2 1 1)
    (gtk-grid-attach-next-to layout height-entry height-label :right 1 1)
    (gtk-grid-attach layout create-button 0 3 1 1)
    layout))

(defun draw-force-setup ()
  (let ((layout (make-instance 'gtk-grid))
        (title (make-instance 'gtk-label
                              :use-markup t
                              :label "<big>Force Setup</big>"))
        ())
    (gtk-grid-attach layout title 0 0 1 1)
    layout))

(defun draw-unit-selection ()
  (let ((layout (make-instance 'gtk-grid))
        (title (make-instance 'gtk-label
                              :use-markup t
                              :label "<big>Unit Selection</big>")))
    (gtk-grid-attach layout title 0 0 1 1)
    layout))

(defun draw-unit-list ()
  (let ((layout (make-instance 'gtk-grid))
        (title (make-instance 'gtk-label
                              :use-markup t
                              :label "<big>Unit List</big>")))
    (gtk-grid-attach layout title 0 0 1 1)
    layout))

(defun draw-gameplay-screen ()
  (let ((layout (make-instance 'gtk-grid
                               :hexpand t
                               :vexpand t
                               :spacing 5))
        (map-scroll (make-instance 'gtk-viewport))
        (map-area (make-instance 'gtk-drawing-area
                                 :hexpand t
                                 :vexpand t))
        (recordsheet (draw-stats)))
    (g-signal-connect map-area "draw"
       (lambda (widget cr)
         (let ((cr (pointer cr))
               ;; Get the GdkWindow for the widget
               (window (gtk-widget-window widget)))
           ;; Clear surface
           (cairo-set-source-rgb cr 1.0 1.0 1.0)
           (cairo-paint cr)
           ;; Example is in 1.0 x 1.0 coordinate space
           ;; Drawing code goes here
           (cairo-set-line-width cr 3)
           (maphash #'(lambda (k v) (cairo-draw-hex k v cr window))
                    (grid/tiles (game/board *game*)))
           (cairo-set-source-rgb cr 1.0 1.0 1.0)
           (map-entities #'(lambda (e)
                             (if (location/q e)
                                 (let ((origin (hex-to-pixel
                                                (get-hex e (game/board *game*))
                                                +default-layout+)))
                                   (cairo-set-source-surface
                                    cr
                                    (cairo-image-surface-create-from-png (display/image-path e))
                                    (+ (* (layout-x-size +default-layout+) -0.9)
                                       (point-x origin))
                                    (+ (* (layout-y-size +default-layout+) -0.8)
                                       (point-y origin)))
                               (cairo-paint cr)))))
           t)))
      (gtk-container-add map-scroll map-area)
      (gtk-grid-attach layout map-scroll 0 0 2 2)
      (gtk-grid-attach-next-to layout recordsheet map-scroll :right 1 1)
    layout))

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
