;;;; megastrike.lisp

(in-package #:megastrike)

(setf *game* (new-game))
(setf *current-layout* :lobby)

(gtk:define-application (:name megastrike
                         :id "bennett.megastrike")
  (gtk:define-main-window (window (gtk:make-application-window :application gtk:*application*))
    (setf (gtk:window-title window) "Megastrike")
    (let ((box (gtk:make-box :orientation gtk:+orientation-vertical+ :spacing 5)))
      (let ((lobby (draw-lobby-screen))
            (game (draw-gameplay-screen)))
        (setf (gtk:widget-hexpand-p lobby) t
              (gtk:widget-vexpand-p lobby) t)
        (setf (gtk:widget-hexpand-p game) t
              (gtk:widget-vexpand-p game) t)
        (gtk:box-append box game)
        (let ((button (gtk:make-button :label "Exit")))
          (gtk:connect button "clicked" (lambda (button)
                                      (declare (ignore button))
                                      (gtk:window-destroy window)))
          (gtk:box-append box button)))
      (setf (gtk:window-child window) box))
    (unless (gtk:widget-visible-p window)
      (gtk:window-present window))))

;;; TODO To draw with Cairo
;;; 3) Write a draw-func that does all the drawing
;;; 4) setf the drawing-area-draw-func to the draw-func

(defun draw-func (area cr width height)
  (declare (ignore area)
           (optimize (speed 3)
                     (debug 0)
                     (safety 0)))
  ;; TODO let some scaling on the size of the picture
  (let ((width (coerce (the fixnum width) 'single-float))
        (height (coerce (the fixnum height) 'single-float))
        (fpi (coerce pi 'single-float)))
    (cairo-draw-hex (new-hexagon :q 1 :r 0 :s -1) cr)
    (cairo-draw-hex (new-hexagon :q 0 :r 0 :s 0) cr)
    (cairo-draw-hex (new-hexagon :q 1 :r 1 :s -2) cr)
    (cairo-draw-hex (new-hexagon :q 0 :r 1 :s -1) cr)))

(defun cairo-draw-hex (hex cr)
  (let ((hex-points (draw-hex hex +default-layout+))
        (hex-center (hex-to-pixel hex +default-layout+)))
    (with-gdk-rgba (color "#00FF00")
        (cairo:move-to (point-x (first hex-points)) (point-y (first hex-points)))
        (dotimes (i 6)
          (cairo:line-to (point-x (nth i hex-points)) (point-y (nth i hex-points))))
        (cairo:close-path)
        (gdk:cairo-set-source-rgba cr color)
        (cairo:fill-preserve))
    (with-gdk-rgba (color "#000000")
        (gdk:cairo-set-source-rgba cr color)
        (cairo:stroke))))
;; (defun cairo-draw-hex (loc hex cr window)
;;   (let ((hex-points (draw-hex hex +default-layout+))
;;         (hex-center (hex-to-pixel hex +default-layout+)))
;;     (cairo-set-source-rgb cr 0.0 0.0 0.0)
;;     (cairo-move-to cr (point-x (first hex-points)) (point-y (first hex-points)))
;;     (dotimes (i 6)
;;       (cairo-line-to cr (point-x (nth i hex-points)) (point-y (nth i hex-points))))
;;     (cairo-close-path cr)
;;     (cairo-stroke-preserve cr)
;;     (cairo-set-source-rgb cr 0.0 0.6 0.09)
;;     (cairo-fill cr)
;;     (cairo-set-source-rgb cr 0.0 0.0 0.0)
;;     (cairo-move-to cr (point-x (nth 3 hex-points))(point-y (nth 3 hex-points)))
;;     (cairo-set-font-size cr 15)
;;     (cairo-text-path cr (format nil "~2,'0D~2,'0D" (first loc) (second loc)))
;;     (cairo-fill cr)))

;; (defun draw-round-report ()
;;   (let ((layout (gtk:make-box :orientation gtk:+orientation-vertical+ :spacing 5)))
;;     ))

(defun draw-gameplay-screen ()
  (let ((map-scroll (gtk:make-scrolled-window))
        (map-area (gtk:make-drawing-area))
       ;; (recordsheet (draw-stats))
       )
    (setf (gtk:drawing-area-content-width map-area) 600
          (gtk:drawing-area-content-height map-area) 800
            (gtk:drawing-area-draw-func map-area) (list (cffi:callback %draw-func)
                                                    (cffi:null-pointer)
                                                    (cffi:null-pointer)))

    ;; (gtk:connect map-area "draw"
    ;;    (lambda (widget cr)
    ;;      (let ((cr (pointer cr))
    ;;            ;; Get the GdkWindow for the widget
    ;;            (window (gtk:widget-window widget)))
    ;;        ;; Clear surface
    ;;        (cairo-set-source-rgb cr 1.0 1.0 1.0)
    ;;        (cairo-paint cr)
    ;;        ;; Example is in 1.0 x 1.0 coordinate space
    ;;        ;; Drawing code goes here
    ;;        (cairo-set-line-width cr 3)
    ;;        (maphash #'(lambda (k v) (cairo-draw-hex k v cr window))
    ;;                 (grid/tiles (game/board *game*)))
    ;;        (cairo-set-source-rgb cr 1.0 1.0 1.0)
    ;;        (map-entities #'(lambda (e)
    ;;                          (if (location/q e)
    ;;                              (let ((origin (hex-to-pixel
    ;;                                             (get-hex e (game/board *game*))
    ;;                                             +default-layout+)))
    ;;                                (cairo-set-source-surface
    ;;                                 cr
    ;;                                 (cairo-image-surface-create-from-png (display/image-path e))
    ;;                                 (+ (* (layout-x-size +default-layout+) -0.9)
    ;;                                    (point-x origin))
    ;;                                 (+ (* (layout-y-size +default-layout+) -0.8)
    ;;                                    (point-y origin)))
    ;;                            (cairo-paint cr)))))
    ;;        t)))
      (setf (gtk:scrolled-window-child map-scroll) map-area)
;;      (gtk:grid-attach-next-to layout recordsheet map-scroll :right 1 1)
    map-scroll))

;; (defun draw-stats ()
;;   (let ((grid (gtk:make-grid)))
;;     (when (game/active-unit *game*)
;;       (let ((unit-id-line (gtk:make-label :str (format nil "<big>~a</big>"
;;                                                        (info/full-name (game/active-unit *game*)))))
;;             (info-line (general-info-block (game/active-unit *game*)))
;;             (attack-line (attack-info-block (game/active-unit *game*)))
;;             (heat-line (heat-info-block (game/active-unit *game*)))
;;             (armor-levels (damage-info-block (game/active-unit *game*)))
;;             (specials (specials-info-block (game/active-unit *game*))))
;;         (gtk:grid-attach grid unit-id-line 0 0 1 1)
;;         (gtk:grid-attach grid info-line 0 1 1 1)
;;         (gtk:grid-attach grid attack-line 0 2 1 1)
;;         (gtk:grid-attach grid heat-line 0 3 1 1)
;;         (gtk:grid-attach grid armor-levels 0 4 1 1)
;;         (gtk:grid-attach grid specials 0 5 1 1)))
;;     grid))
