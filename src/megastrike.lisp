;;;; megastrike.lisp

(in-package #:megastrike)

(setf *current-layout* :lobby)

(gtk:define-application (:name megastrike
                         :id "bennett.megastrike")
  (gtk:define-main-window (window (gtk:make-application-window :application gtk:*application*))
    (setf (gtk:window-title window) "Megastrike")
    (setf *game* (new-game))
    (let ((box (gtk:make-box :orientation gtk:+orientation-vertical+ :spacing 5))
          (command-buttons (gtk:make-box :orientation gtk:+orientation-horizontal+ :spacing 5)))
      (let ((lobby (draw-lobby-screen)))
        (setf (gtk:widget-hexpand-p lobby) t
              (gtk:widget-vexpand-p lobby) t)
        (gtk:box-append box lobby)

        (let ((button (gtk:make-button :label "Not Ready")))
          (setf (gtk:widget-sensitive-p button) nil)
          (gtk:connect button "clicked" (lambda (button)
                                          (declare (ignore button))
                                          (setf (game/units *game*) (string-list/source (lobby/units *lobby*))
                                                (game/forces-hash *game*) (string-list/source (lobby/forces *lobby*))
                                                (game/board *game*) (lobby/map *lobby*))
                                          (gtk:box-remove box lobby)
                                          (let ((game (draw-gameplay-screen)))
                                            (setf (gtk:widget-hexpand-p game) t
                                                  (gtk:widget-vexpand-p game) t)
                                            (gtk:box-prepend box game))))
          (gtk:timeout-add 500 (lambda ()
                                 (if (game-ready-p)
                                     (progn
                                       (setf (gtk:button-label button) "Game Ready")
                                       (setf (gtk:widget-sensitive-p button) t)
                                       glib:+source-remove+)
                                     glib:+source-continue+)))
          (gtk:box-append command-buttons button)))

        (let ((button (gtk:make-button :label "Exit")))
          (gtk:connect button "clicked" (lambda (button)
                                      (declare (ignore button))
                                      (gtk:window-destroy window)))
          (gtk:box-append command-buttons button))

      (setf (gtk:window-child window) box)
      (gtk:box-append box command-buttons))
    (unless (gtk:widget-visible-p window)
      (gtk:window-present window))))

(declaim (ftype (function (t t t t) t) draw-func))

(cffi:defcallback %draw-func :void ((area :pointer)
                                    (cr :pointer)
                                    (width :int)
                                    (height :int)
                                    (data :pointer))
  (declare (ignore data))
  (let ((cairo:*context* (make-instance 'cairo:context
                                        :pointer cr
                                        :width width
                                        :height height
                                        :pixel-based-p nil)))
    (draw-func (make-instance 'gir::object-instance
                              :class (gir:nget gtk:*ns* "DrawingArea")
                              :this area)
               (make-instance 'gir::struct-instance
                              :class (gir:nget megastrike::*ns* "Context")
                              :this cr)
               width height)))

(defun draw-func (area cr width height)
  (declare (ignore area)
           (optimize (speed 3)
                     (debug 0)
                     (safety 0)))
  ;; TODO let some scaling on the size of the picture
  (let ((width (coerce (the fixnum width) 'single-float))
        (height (coerce (the fixnum height) 'single-float))
        (fpi (coerce pi 'single-float)))
    (loop :for loc being the hash-keys of (board/tiles (game/board *game*))
          :for tile being the hash-values of (board/tiles (game/board *game*))
          :do (cairo-draw-hex loc tile cr))))

(defun cairo-draw-hex (loc hex cr)
  (let ((hex-points (draw-hex hex +default-layout+))
        (hex-center (hex-to-pixel hex +default-layout+)))
    (with-gdk-rgba (color "#009917")
        (cairo:move-to (point-x (first hex-points)) (point-y (first hex-points)))
        (dotimes (i 6)
          (cairo:line-to (point-x (nth i hex-points)) (point-y (nth i hex-points))))
        (cairo:close-path)
        (gdk:cairo-set-source-rgba cr color)
        (cairo:fill-preserve))
    (with-gdk-rgba (color "#000000")
        (gdk:cairo-set-source-rgba cr color)
        (cairo:stroke))
    (with-gdk-rgba (color "#000000")
      (cairo:move-to (point-x (nth 3 hex-points)) (point-y (nth 3 hex-points)))
      (cairo:set-font-size 15)
      (cairo:text-path (format nil "~2,'0D~2,'0D" (first loc) (second loc)))
      (cairo:fill-path))))

;; (defun draw-round-report ()
;;   (let ((layout (gtk:make-box :orientation gtk:+orientation-vertical+ :spacing 5)))
;;     ))

(defun draw-gameplay-screen ()
  (let ((layout (gtk:make-box :orientation gtk:+orientation-horizontal+ :spacing 3))
        (map-scroll (gtk:make-scrolled-window))
        (map-area (gtk:make-drawing-area))
        (clicker (gtk:make-gesture-click))
        (recordsheet (draw-recordsheets))
       )
    (setf (gtk:drawing-area-content-width map-area) (+ (* (board/width (game/board *game*))
                                                          (layout-x-size +default-layout+))
                                                       (* (layout-x-origin +default-layout+) 2))
          (gtk:drawing-area-content-height map-area) (+ (* (board/height (game/board *game*))
                                                          (layout-y-size +default-layout+))
                                                       (* (layout-y-origin +default-layout+) 2))
          (gtk:drawing-area-draw-func map-area) (list (cffi:callback %draw-func)
                                                      (cffi:null-pointer)
                                                      (cffi:null-pointer)))
    (setf (gtk:scrolled-window-child map-scroll) map-area)
    (setf (gtk:widget-hexpand-p map-scroll) t
          (gtk:widget-vexpand-p map-scroll) t)
    (gtk:widget-add-controller map-area clicker)
    (gtk:connect clicker "pressed"
                 (lambda (handler presses x y)
                   (declare (ignore handler presses))
                   (let ((hex (pixel-to-hex (make-point x y) +default-layout+)))
                     (format t "Clicked at ~a,~a,~a: ~a~%" (hexagon-q hex) (hexagon-r hex) (hexagon-s hex) (offset-from-hex hex)))))
    (gtk:box-append layout map-scroll)
    (gtk:box-append layout recordsheet)
    layout))

