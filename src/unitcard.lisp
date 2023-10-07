(in-package :megastrike)

(defun draw-recordsheets ()
  (let ((layout (gtk:make-box :orientation gtk:+orientation-vertical+ :spacing 15))
        (recordsheet-label (gtk:make-label :str "Record Sheets"))
        (scroll-box (gtk:make-scrolled-window))
        (record-sheets (gtk:make-list-box)))
    (gtk:box-append layout recordsheet-label)
    (gtk:box-append layout scroll-box)
    (setf (gtk:widget-vexpand-p scroll-box) t
          (gtk:widget-hexpand-p scroll-box) t
          (gtk:widget-vexpand-p record-sheets) t
          (gtk:widget-hexpand-p record-sheets) t)
    (setf (gtk:scrolled-window-child scroll-box) record-sheets)
    (gtk:connect record-sheets "row-selected"
                 (lambda (lb row)
                   (declare (ignore lb))
                   (when row
                     (let* ((unit (gtk:frame-label (gobj:coerce (gtk:list-box-row-child row) 'gtk:frame)))
                            (unit-list (alexandria:hash-table-values (game/units *game*))))
                       (setf (game/active-unit *game*) (car (member unit unit-list
                                                                    :key #'cu/full-name
                                                                    :test #'string=)))
                       (format t "~A~%" (cu/full-name (game/active-unit *game*)))))))
    (maphash #'(lambda (uuid unit)
                (declare (ignore uuid))
                (gtk:list-box-append record-sheets (draw-stat-block unit)))
            (game/units *game*))
    layout))

(defun draw-stat-block (u)
  (let ((frame (gtk:make-frame :label (cu/full-name u)))
        (statblock (gtk:make-box :orientation gtk:+orientation-vertical+ :spacing 5))
        (info-line (draw-general-info-line u))
        (attack-line (draw-attack-line u))
        (heat-line (draw-heat-line u))
        (damage-line (draw-damage-line u))
        (abilities-line (draw-abilities-line u)))
    (gtk:box-append statblock info-line)
    (gtk:box-append statblock attack-line)
    (gtk:box-append statblock heat-line)
    (gtk:box-append statblock damage-line)
    (gtk:box-append statblock abilities-line)
    (setf (gtk:frame-child frame) statblock)
    frame))

(defun draw-general-info-line (u)
  (let ((line (gtk:make-box :orientation gtk:+orientation-horizontal+ :spacing 5))
        (type-label (gtk:make-label :str (format nil "Type: ~A" (mek/type (cu/mek u)))))
        (size-label (gtk:make-label :str (format nil "Size: ~A" (mek/size (cu/mek u)))))
        (tmm-label (gtk:make-label :str (format nil "TMM: ~A" (mek/tmm (cu/mek u)))))
        (move-label (gtk:make-label :str (format nil "Move: ~A" (print-movement (cu/mek u)))))
        (role-label (gtk:make-label :str (format nil "Role: ~A" (mek/role (cu/mek u)))))
        (pilot-label (gtk:make-label :str (format nil "Pilot: ~A" (display (cu/pilot u))))))
    (gtk:box-append line type-label)
    (gtk:box-append line size-label)
    (gtk:box-append line tmm-label)
    (gtk:box-append line move-label)
    (gtk:box-append line role-label)
    (gtk:box-append line pilot-label)
    line))

(defun draw-attack-line (u)
  (let ((line (gtk:make-box :orientation gtk:+orientation-horizontal+ :spacing 5))
        (attack-label (gtk:make-label :str (cu/attack-string u))))
    (gtk:box-append line attack-label)
    line))



(defun draw-heat-line (u)
  (let ((line (gtk:make-box :orientation gtk:+orientation-horizontal+ :spacing 5))
        (ov-label (gtk:make-label :str (format nil "<b>OV:</b> ~a" (mek/ov (cu/mek u)))))
        (heat-label (gtk:make-label :str (format nil "<b>Current Heat:</b> ~a" (cu/cur-heat u))))
        (heat-level-bar (gtk:make-level-bar)))
    (setf (gtk:label-use-markup-p ov-label) t
          (gtk:label-use-markup-p heat-label) t)
    (setf (gtk:level-bar-mode heat-level-bar) gtk:+level-bar-mode-discrete+)
    (setf (gtk:level-bar-min-value heat-level-bar) 0d0
          (gtk:level-bar-max-value heat-level-bar) 4d0
          (gtk:level-bar-value heat-level-bar) (float (cu/cur-heat u) 0d0))
    (gtk:widget-add-css-class heat-level-bar "heat-bar")
    (gtk:box-append line ov-label)
    (gtk:box-append line heat-label)
    (gtk:box-append line heat-level-bar)
    line))

(defun draw-damage-line (u)
  (let ((frame (gtk:make-frame :label "Damage"))
        (layout (gtk:make-grid))
        (armor-label (gtk:make-label :str "Armor: "))
        (armor-level-bar (gtk:make-level-bar))
        (struct-label (gtk:make-label :str "Structure: "))
        (struct-level-bar (gtk:make-level-bar)))
    (setf (gtk:level-bar-min-value armor-level-bar) 0d0
          (gtk:level-bar-max-value armor-level-bar) (float (mek/armor (cu/mek u)) 0d0)
          (gtk:level-bar-min-value struct-level-bar) 0d0
          (gtk:level-bar-max-value struct-level-bar) (float (mek/structure (cu/mek u)) 0d0)
          (gtk:level-bar-mode armor-level-bar) gtk:+level-bar-mode-discrete+
          (gtk:level-bar-mode struct-level-bar) gtk:+level-bar-mode-discrete+
          (gtk:level-bar-value armor-level-bar) (float (cu/cur-armor u) 0d0)
          (gtk:level-bar-value struct-level-bar) (float (cu/cur-struct u) 0d0))
    (gtk:widget-add-css-class armor-level-bar "damage-bar")
    (gtk:widget-add-css-class struct-level-bar "damage-bar")
    (gtk:grid-attach layout armor-label 0 0 1 1)
    (gtk:grid-attach layout armor-level-bar 1 0 3 1)
    (gtk:grid-attach layout struct-label 0 1 1 1)
    (gtk:grid-attach layout struct-level-bar 1 1 3 1)
    (setf (gtk:frame-child frame) layout)
    frame))

(defun draw-abilities-line (u)
  (let ((abilities-label (gtk:make-label :str (format nil "Abilities: ~a" (mek/abilities (cu/mek u))))))
    abilities-label))
