(in-package :megastrike)

;; (defun general-info-block (combat-unit)
;;   "Draws the first block of the Record sheet, containing the Type, MV, Role, and Pilot info."
;;   (let ((u-type  (info/unit-type combat-unit))
;;         (u-size  (info/size combat-unit))
;;         (u-tmm   (unit-tmm combat-unit))
;;         (u-move  (moveable/format-move combat-unit))
;;         (u-role  (info/role combat-unit))
;;         (u-pilot (pilot/display combat-unit)))
;;     (let ((frame (make-instance 'gtk-frame
;;                                 :border-width 2
;;                                 :label "General Info"))
;;           (info-label (make-instance 'gtk-label
;;                                      :use-markup t
;;                                      :halign :start
;;                                      :label (format nil "<b>TP:</b> ~a~4@t<b>Size:</b> ~a~4@t<b>TMM:</b>~a~4@t<b>Move:</b> ~a~%<b>Role:</b> ~16a <b>Pilot:</b>~a"
;;             u-type u-size u-tmm u-move u-role u-pilot))))
;;       (gtk-container-add frame info-label)
;;       frame)))

;; (defun attack-info-block (combat-unit)
;;   "Draws the attack and heat information."
;;   (let ((frame (make-instance 'gtk-frame
;;                               :border-width 2
;;                               :label "Attacks"))
;;         (atk-label (make-instance 'gtk-label
;;                                   :halign :start
;;                                   :label (format nil "S: ~3a M: ~3a L: ~3a"
;;                                                  (attacks/short combat-unit)
;;                                                  (attacks/medium combat-unit)
;;                                                  (attacks/long combat-unit)))))
;;     (gtk-container-add frame atk-label)
;;     frame))

;; (defun heat-info-block (combat-unit)
;;   (let ((frame (make-instance 'gtk-frame
;;                               :border-width 2
;;                               :label "Heat"))

;;         (provider (gtk-css-provider-new))
;;         (grid (gtk-grid-new))
;;         (ov-label (make-instance 'gtk-label
;;                                  :label (format nil "<b>OV: </b> ~a"
;;                                                 (heat/ov combat-unit))
;;                                  :use-markup t))
;;         (heat-label (make-instance 'gtk-label
;;                                    :label "<b>Current Heat: </b>"
;;                                    :use-markup t))
;;         (heat-level-bar (make-instance 'gtk-level-bar
;;                                       :orientation :horizontal
;;                                       :mode :discrete
;;                                       :min-value 0
;;                                       :max-value 4
;;                                       :height-request 20
;;                                       :value (heat/cur-heat combat-unit))))
;;     (gtk-css-provider-load-from-path provider (namestring (uiop:merge-pathnames* "data/css/heatbar.css" *here*)))
;;     (gtk-style-context-add-provider (gtk-widget-get-style-context heat-level-bar)
;;                                     provider
;;                                     +gtk-style-provider-priority-application+)
;;     (gtk-grid-attach grid ov-label 0 0 1 1)
;;     (gtk-grid-attach grid heat-label 0 1 1 1)
;;     (gtk-grid-attach grid heat-level-bar 1 1 1 1)
;;     (gtk-container-add frame grid)
;;     frame))

;; (defun damage-info-block (combat-unit)
;;   "Draws the current damage and structure and any critical hits."
;;   (let ((ca (damageable/cur-armor combat-unit))
;;         (ma (damageable/max-armor combat-unit))
;;         (cs (damageable/cur-struct combat-unit))
;;         (ms (damageable/max-struct combat-unit)))
;;     (let ((frame (make-instance 'gtk-frame
;;                                 :border-width 2
;;                                 :label "Damage"))
;;           (provider (gtk-css-provider-new))
;;           (armor-label (make-instance 'gtk-label
;;                                       :label "Armor: "))
;;           (armor-level-bar (make-instance 'gtk-level-bar
;;                                           :orientation :horizontal
;;                                           :mode :discrete
;;                                           :min-value 0
;;                                           :max-value ma
;;                                           :hexpand nil
;;                                           :halign :start
;;                                           :width-request (* ma 20)
;;                                           :height-request 20
;;                                           :value ca))
;;           (struct-label (make-instance 'gtk-label
;;                                        :label "Structure: "))
;;           (struct-level-bar (make-instance 'gtk-level-bar
;;                                            :orientation :horizontal
;;                                            :mode :discrete
;;                                            :min-value 0
;;                                            :max-value ms
;;                                            :hexpand nil
;;                                            :halign :start
;;                                            :height-request 20
;;                                            :width-request (* ms 20)
;;                                            :value cs))
;;           (grid (make-instance 'gtk-grid
;;                                :column-spacing 10)))
;;       (gtk-css-provider-load-from-path provider (namestring (uiop:merge-pathnames* "data/css/damagebars.css" *here*)))
;;       (gtk-style-context-add-provider (gtk-widget-get-style-context armor-level-bar)
;;                                       provider
;;                                       +gtk-style-provider-priority-application+)
;;       (gtk-style-context-add-provider (gtk-widget-get-style-context struct-level-bar)
;;                                       provider
;;                                       +gtk-style-provider-priority-application+)
;;       (gtk-grid-attach grid armor-label 0 0 1 1)
;;       (gtk-grid-attach-next-to grid armor-level-bar armor-label :right 1 1)
;;       (gtk-grid-attach grid struct-label 0 1 1 1)
;;       (gtk-grid-attach-next-to grid struct-level-bar struct-label :right 1 1)
;;       (gtk-container-add frame grid)
;;       frame)))

;; (defun specials-info-block (combat-unit)
;;   "Draws the Specials"
;;   (let ((frame (make-instance 'gtk-frame
;;                               :border-width 2
;;                               :label "Damage"))
;;         (specials-label (make-instance 'gtk-label
;;                                        :label (format nil "Specials: ~{~a~^, ~}"
;;                                                       (specials/special-list combat-unit)))))
;;     (gtk-container-add frame specials-label)
;;     frame))
