(in-package :megastrike)

(defmethod display-round-report ((frame megastrike) stream)
  (write-string (game/phase-log *game*) stream)
  (incf (game/current-phase *game*))
  (if (= (game/current-phase *game*) 5)
      (setf (game/current-phase *game*) 0))
  (terpri stream)
  (let ((done-button (make-pane 'push-button
                                :label "Done"
                                :activate-callback
                                #'(lambda (g)
                                    (do-phase frame)))))
    (with-output-as-gadget (stream)
    done-button)))

(defmethod display-overview ((frame megastrike) stream)
  (format stream "Current Force List~%")
  (formatting-table (stream)
    (formatting-row (stream)
      (formatting-cell (stream)
        (format stream "Force"))
      (formatting-cell (stream)
        (format stream "Color")))
    (dolist (f (game/forces *game*))
      (formatting-row (stream)
        (formatting-cell (stream)
          (present f 'force :stream stream))
      (formatting-cell (stream)
        (draw-rectangle* stream 0 0 30 30 :ink (force/color f))))))
  (terpri stream)
  (let ((force-text (make-pane 'text-field :width 200
                                          :foreground +black+
                                          :background +white+
                                          :value "AFFS"))
        (force-color (make-pane 'list-pane
                               :items (mapcar #'car +color-list+))))
    (format stream "Force Name:    ")
    (with-output-as-gadget (stream)
      force-text)
    (terpri stream)
    (surrounding-output-with-border (stream :ink +grey30+ :shape :rounded)
      (with-output-as-gadget (stream)
        force-color))
    (terpri stream)
    (with-output-as-gadget (stream)
      (let ((new-force-button (make-pane 'push-button
                                        :label "New Force"
                                        :activate-callback
                                        #'(lambda (gadget)
                                            (add-force *game*
                                                       (new-force
                                                        (gadget-value force-text)
                                                        (cdr (assoc
                                                              (gadget-value force-color)
                                                              +color-list+))))
                                            (redisplay-frame-panes frame)))))
        new-force-button)))
  (terpri stream)
  (terpri stream)
  (terpri stream)
  (let ((width  (make-pane 'text-field :width 50 :value "16"))
        (height (make-pane 'text-field :width 50 :value "17")))
    (write-string "Map Settings" stream)
    (terpri stream)
    (write-string "Width: ")
    (with-output-as-gadget (stream)
      width)
    (terpri stream)
    (write-string "Height: ")
    (with-output-as-gadget (stream)
      height)
    (with-output-as-gadget (stream)
      (let ((update-map-button (make-pane 'push-button
                                          :label "Update Map Size"
                                          :activate-callback
                                          #'(lambda (gadget)
                                              (setf (game/board *game*)
                                                    (make-grid (parse-integer (gadget-value width))
                                                               (parse-integer (gadget-value height))))
                                              (redisplay-frame-panes frame)))))
        update-map-button)))
  (terpri stream)
    (let ((forces-ready (> (length (game/forces *game*)) 1))
          (map-ready (> (hash-table-count (tiles (game/board *game*))) 1)))
      (if (and forces-ready map-ready)
          (with-output-as-gadget (stream)
            (let ((launch-game-button (make-pane
                               'push-button
                               :label "Game Ready"
                               :activate-callback
                               #'(lambda (g)
                                   (do-initiative-phase frame)))))
              launch-game-button)))))

(defmethod display-lobby-force-list ((frame megastrike) stream)
  (if (= 0 (length (beast:all-entities)))
      (write-string "Force has no units yet." stream)
      (formatting-table (stream)
        (formatting-row (stream)
          (formatting-cell (stream) (write-string "Unit Name" stream))
          (formatting-cell (stream) (write-string "PV" stream))
          (formatting-cell (stream) (write-string "Pilot Name" stream))
          (formatting-cell (stream) (write-string "Skill" stream))
          (formatting-cell (stream) (write-string "Starting Hex" stream)))
        (run-list-force))))

(defmethod display-lobby-detail-view ((frame megastrike) stream)
  (let* ((pname  (make-pane 'text-field :width 200 :value "Test Pilot"))
         (pskill (make-pane 'text-field :width 50 :value "4")))
    (formatting-table (stream)
      (formatting-row (stream)
        (formatting-cell (stream) (format stream "Pilot name: "))
        (formatting-cell (stream) (with-output-as-gadget (stream) pname))
        (formatting-cell (stream) (format stream "Pilot skill: "))
        (formatting-cell (stream) (with-output-as-gadget (stream) pskill))))
    (terpri stream)
    (with-output-as-gadget (stream)
      (let ((add-unit-button
              (make-pane 'push-button
                         :label "Add Unit"
                         :activate-callback
                         #'(lambda (g)
                             (add-unit (game/selected-force *game*)
                                       (new-element-from-mul
                                        (lobby/selected-mek *lobby*)
                                        :pname (gadget-value pname)
                                        :pskill (parse-integer (gadget-value pskill))))
                             (redisplay-frame-panes frame)))))
      add-unit-button)))
  (terpri stream)
  (let ((meks *master-unit-list*))
    (formatting-table (stream)
      (formatting-row (stream)
        (formatting-cell (stream) (write-string "Unit name" stream))
        (formatting-cell (stream) (write-string "PV" stream))
        (formatting-cell (stream) (write-string "Size" stream))
        (formatting-cell (stream) (write-string "Move" stream))
        (formatting-cell (stream) (write-string "S/M/L" stream))
        (formatting-cell (stream) (write-string "OV" stream))
        (formatting-cell (stream) (write-string "A/S" stream))
        (formatting-cell (stream) (write-string "Specials" stream)))
    (dolist (m meks)
      (if (and (lobby/selected-mek *lobby*)
               (string= (mek/short-name m)
                        (mek/short-name (lobby/selected-mek *lobby*))))
          (with-text-style (stream *selected-text-style*)
            (present m 'mek :stream stream))
          (present m 'mek :stream stream))))))

(defmethod display-map ((frame megastrike) stream)
  (maphash (lambda (k v)
             (declare (ignorable k))
             (present v 'tile :stream stream))
           (tiles (game/board *game*)))
  (run-draw-units))

(defmethod display-record-sheet ((frame megastrike) stream)
  (with-text-style (stream (make-text-style :serif :bold :large))
     (format stream "Turn: ~8a Phase: ~a~%"
             (game/turn-number *game*) (nth (game/current-phase *game*) *phase-order*)))
  (if (game/active-unit *game*) (unit-detail stream (game/active-unit *game*)))
  (terpri stream)
  (format stream "~a" (game/initiative-list *game*)))

(defmethod display-quickstats ((frame megastrike) stream)
  (run-show-quickstats))
