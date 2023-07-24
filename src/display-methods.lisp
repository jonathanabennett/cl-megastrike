(in-package :megastrike)

(defmethod display-round-report ((frame megastrike) stream)
  (write-string (phase-log frame) stream)
  (let ((done-button (make-pane 'push-button
                                :label "Done"
                                :activate-callback #'(lambda (g)
                                                       (setf (frame-current-layout (*application-frame*))
                                                             :game-board)))))
    (with-output-as-gadget (stream)
    done-button)))

(defmethod display-overview ((frame megastrike) stream)
  (format stream "Current Army List~%")
  (formatting-table (stream)
    (formatting-row (stream)
      (formatting-cell (stream)
        (format stream "Army"))
      (formatting-cell (stream)
        (format stream "Color")))
    (dolist (a (frame/armies *application-frame*))
      (formatting-row (stream)
        (formatting-cell (stream)
          (present a 'army :stream stream))
      (formatting-cell (stream)
        (draw-rectangle* stream 0 0 30 30 :ink (army/color a))))))
  (terpri stream)
  (let ((army-text (make-pane 'text-field :width 200
                                          :foreground +black+
                                          :background +white+
                                          :value "AFFS"))
        (army-color (make-pane 'list-pane
                               :items (mapcar #'car +color-list+))))
    (format stream "Army Name:    ")
    (with-output-as-gadget (stream)
      army-text)
    (terpri stream)
    (surrounding-output-with-border (stream :ink +grey30+ :shape :rounded)
      (with-output-as-gadget (stream)
        army-color))
    (terpri stream)
    (with-output-as-gadget (stream)
      (let ((new-army-button (make-pane 'push-button
                                        :label "New Army"
                                        :activate-callback #'(lambda (gadget)
                                                               (new-army (gadget-value army-text)
                                                                       (cdr (assoc (gadget-value army-color) +color-list+)))
                                                               (redisplay-frame-panes *application-frame*)))))
        new-army-button)))
  (terpri stream)
  (terpri stream)
  (terpri stream)
  (let ((width 16)
        (height 17)
        (width-gadget (make-pane 'text-field :width 50
                                 :value "16"
                                 :value-changed-callback
                                 #'(lambda (g v)
                                     (if (not (string= "" v))
                                              (setf width (parse-integer v))))))
        (height-gadget (make-pane 'text-field :width 50
                                  :value "17"
                                  :value-changed-callback
                                  #'(lambda (g v)
                                      (if (not (string= "" v))
                                               (setf height (parse-integer v)))))))
    (write-string "Map Settings" stream)
    (terpri stream)
    (write-string "Width: ")
    (with-output-as-gadget (stream)
      width-gadget)
    (terpri stream)
    (write-string "Height: ")
    (with-output-as-gadget (stream)
      height-gadget)
    (with-output-as-gadget (stream)
      (let ((update-map-button (make-pane 'push-button
                                          :label "Update Map Size"
                                          :activate-callback #'(lambda (gadget)
                                                                 (setf (frame/game-board *application-frame*)
                                                                       (make-grid width height))))))
        update-map-button)))
  (terpri stream)
  (with-output-as-gadget (stream)
    (let ((armies-ready (> (length (frame/armies *application-frame*)) 1))
          (map-ready (> (hash-table-count (tiles (frame/game-board *application-frame*))) 1))
          (launch-game-button (make-pane
                               'push-button
                               :label "Game Not Ready"
                               :activate-callback
                               #'(lambda (g)
                                   (setf (frame-current-layout *application-frame*) :game-round)))))
      (if (and armies-ready map-ready)
          (setf (gadget-label launch-game-button) "Game Ready"))
      launch-game-button)))

(defmethod display-lobby-army-list ((frame megastrike) stream)
  (if (= 0 (length (beast:all-entities)))
      (write-string "Army has no units yet." stream)
      (formatting-table (stream)
        ;; TODO Add Header Row
        (formatting-row (stream)
          (formatting-cell (stream) (write-string "Unit Name" stream))
          (formatting-cell (stream) (write-string "PV" stream))
          (formatting-cell (stream) (write-string "Pilot Name" stream))
          (formatting-cell (stream) (write-string "Skill" stream))
          (formatting-cell (stream) (write-string "Starting Hex" stream)))
        (run-list-army))))

(defmethod display-lobby-detail-view ((frame megastrike) stream)
  (let* ((pname "Test Pilot")
         (pname-gadget  (make-pane
                         'text-field :width 200 :value "Test Pilot"
                                   :value-changed-callback #'(lambda (g v)
                                                               (setf pname v))))
         (pskill 4)
         (pskill-gadget (make-pane
                         'text-field
                         :width 50 :value "4"
                         :value-changed-callback #'(lambda (g v)
                                                     (if (not (string= "" v))
                                                         (setf pskill (parse-integer v))))))
         (xpos 1)
         (xpos-gadget   (make-pane
                         'text-field
                         :width 20 :value "4"
                         :value-changed-callback #'(lambda (g v)
                                                     (if (not (string= "" v))
                                                         (setf xpos (parse-integer v))))))
         (ypos 1)
         (ypos-gadget   (make-pane
                         'text-field
                         :width 20 :value "4"
                         :value-changed-callback #'(lambda (g v)
                                                     (if (not (string= "" v))
                                                         (setf ypos (parse-integer v)))))))
    (formatting-table (stream)
      (formatting-row (stream)
        (formatting-cell (stream) (format stream "Pilot name: "))
        (formatting-cell (stream) (with-output-as-gadget (stream) pname-gadget))
        (formatting-cell (stream) (format stream "Pilot skill: "))
        (formatting-cell (stream) (with-output-as-gadget (stream) pskill-gadget)))
    (formatting-row (stream)
      (formatting-cell (stream) (write-string "Deployment Location" stream)))
    (formatting-row (stream)
      (formatting-cell (stream) (write-string "X: " stream))
    (formatting-cell (stream) (with-output-as-gadget (stream) xpos-gadget))
    (formatting-cell (stream) (write-string "Y: " stream))
    (formatting-cell (stream) (with-output-as-gadget (stream) ypos-gadget))))
    (with-output-as-gadget (stream)
      (let ((add-unit-button
              (make-pane 'push-button
                         :label "Add Unit"
                         :activate-callback
                         #'(lambda (g)
                             (add-unit (lobby/selected-army *application-frame*)
                                       (new-element-from-mul
                                        (lobby/selected-mek *application-frame*)
                                        :pname pname :pskill pskill :x xpos :y ypos))
                             (redisplay-frame-panes *application-frame*)))))
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
      (if (and (lobby/selected-mek *application-frame*)
               (string= (mek/short-name m)
                        (mek/short-name (lobby/selected-mek *application-frame*))))
          (with-text-style (stream *selected-text-style*)
            (present m 'mek :stream stream))
          (present m 'mek :stream stream))))))

(defmethod display-map ((frame megastrike) stream)
  (maphash (lambda (k v)
             (declare (ignorable k))
             (present v 'tile :stream stream))
           (tiles (frame/game-board *application-frame*)))
  (run-draw-units))

(defmethod display-record-sheet ((frame megastrike) stream)
  (with-text-style (stream (make-text-style :serif :bold :large))
     (format stream "Turn: ~8a Phase: ~a~%"
             (turn-number frame) (nth (current-phase frame) *phase-order*)))
  (if (active-unit frame) (unit-detail stream (active-unit frame)))
  (terpri stream)
  (format stream "~a" (initiative-list *application-frame*)))

(defmethod display-quickstats ((frame megastrike) stream)
  (run-show-quickstats))
