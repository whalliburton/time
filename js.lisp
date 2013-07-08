(in-package :web-js)

(defparameter *time-js-file*
  (ps*
   '(progn

     (defun set-shortcut-move (target key id)
       (set-shortcut-fn target key (lambda (event) (move-to id))))

     (defun setup-navigation (target prefix left right)
       (let ((el (get-by-id target)))
         (setf (slot-value el 'navigation-elements) (collect-children-with-prefix el prefix))
         (when left (listen el ((@ shortcut build) "left" (lambda (event) (move-to left)))))
         (when right (listen el ((@ shortcut build) "right" (lambda (event) (move-to right)))))
         (listen el ((@ shortcut build) "up" (lambda (event) (handle-navigation el event true))))
         (listen el ((@ shortcut build) "down" (lambda (event) (handle-navigation el event false))))
         (listen el ((@ shortcut build) "tab" (lambda (event) (handle-navigation el event false))))
         (listen el ((@ shortcut build) "enter" (lambda (event) (handle-navigation-enter el event))))
         (listen el (lambda (event) (handle-navigation-enter el event)) "click")))

     (defun focus (el)
       ((@ el focus)))

     (defun move-to (id)
       (let* ((el (get-by-id id))
              (type (@ el node-name)))
         (if (== type "TABLE")
           (focus (@ (@ (aref el.rows 0) first-child)))
           (focus el))))

     (defun handle-navigation (el event up)
       (let* ((elements (slot-value el 'navigation-elements))
              (last (1- (@ elements length)))
              (current (@ document active-element))
              (index ((@ elements index-of) current))
              (next (aref elements
                          (if up
                            (if (= index 0)
                              last
                              (1- index))
                            (if (= index last)
                              0
                              (1+ index))))))
         (focus next)))

     (defun handle-navigation-enter (el event)
       (unless (has-class (@ document active-element) "unselected")
         (request "selection" (create :element (@ (@ document active-element) id)))))

     )))

(defun time-js-file () *time-js-file*)
