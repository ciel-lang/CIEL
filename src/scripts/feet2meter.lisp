

#++
(ql:quickload '("str" "log4cl"))

(defpackage nodgui-example
  (:use :cl
        :nodgui))
        ;; :nodgui-lite))

(in-package :nodgui-example)

(defun calculate (feet-widget meter-widget)
  (let ((feet-input (text feet-widget)))
    ;; (when (and feet-input (str:non-blank-string-p feet-input))
    (when (and feet-input)
      ;; (log:info feet-input)
      (setf (text meter-widget) (format nil "~,2F" (*
                                                    (read-from-string feet-input)
                                                    0.3048d0))))))

;; error ??
;; "The function NODGUI-EXAMPLE::FEET-TO-METERS is undefined."
;; => <2026-01-16> fixed in nodgui master


(defun feet-to-meters (&key debug (theme "yaru"))
  (let ((nodgui:*debug-tk* debug))
    (with-nodgui ()

      ;; we can now use the yaru theme by default.
      ;; (use-theme theme)

      (wm-title *tk* (format nil "Feet to Meters - theme ~a" theme))
      (let ((c (make-instance 'frame)))
        (grid c 0 0 :sticky "ne")
        (grid-columnconfigure *tk* 0 :weight 1)
        (grid-rowconfigure *tk* 0 :weight 1)
        (let* ((c.feet (grid (make-instance 'entry :width 7)
                             1 2 :sticky "we" :padx 5 :pady 5))
               (c.meters (grid (make-instance 'entry :state "readonly")
                               2 2 :sticky "we" :padx 5 :pady 5)))
          (grid (make-instance 'button
                               :text "Calculate"
                               :command (lambda () (calculate c.feet c.meters)))
                3 3 :sticky "w" :padx 5 :pady 5)
          (grid (make-instance 'label :text "feet")
                1 3 :sticky "w" :padx 5 :pady 5)
          (grid (make-instance 'label :text "is equivalent to")
                2 1 :sticky "w" :padx 5 :pady 5)
          (grid (make-instance 'label :text "meters")
                2 3 :sticky "w" :padx 5 :pady 5))))))

#+ciel
(print :by)


#+ciel
(feet-to-meters)
