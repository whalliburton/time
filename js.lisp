(in-package :web-js)

(defparameter *time-js-file*
  (ps*
   '(progn
     (defun listen (element callback)
       ((@ element add-event-listener) "keydown" callback))

     (defun numberp (el)
       (return (=== (typeof el) "number")))

     (defun build-shortcut (key callback)
       (return
         (lambda (e)
           (let* ((e (or e (@ window event)))
                  (code (or (@ e key-code) (@ e which))))
             (console :key code)
             (when (= key code) (callback e))))))

     (defun set-shortcut-move (target key id)
       (let* ((el (get-by-id target))
              (fn (lambda (event) (move-to id))))
         (listen
          el
          (if (numberp key)
            (build-shortcut key fn)
            ((@ shortcut build) key fn)))))

     (defun setup-navigation (target prefix left right)
       (let ((el (get-by-id target)))
         (setf (slot-value el 'navigation-elements) (collect-children-with-prefix el prefix))
         (when left (listen el ((@ shortcut build) "left" (lambda (event) (move-to left)))))
         (when right (listen el ((@ shortcut build) "right" (lambda (event) (move-to right)))))
         (listen el ((@ shortcut build) "up" (lambda (event) (handle-navigation el event true))))
         (listen el ((@ shortcut build) "down" (lambda (event) (handle-navigation el event false))))
         (listen el ((@ shortcut build) "enter" (lambda (event) (handle-navigation-enter el event))))))

     (defun focus (el)
       ((@ el focus)))

     (defun move-to (id)
       (let* ((el (get-by-id id))
              (type (@ el node-name)))
         (if (== type "INPUT")
           ((@ el focus))
           (if (== type "TABLE")
             (if (slot-value el 'current-selection)
               (focus (@ (slot-value el 'current-selection)))
               (if (slot-value el 'current-focus)
                 (focus (@ (slot-value el 'current-focus)))
                 (focus (@ (@ (aref el.rows 0) first-child)))))))))

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
         (setf (slot-value el 'current-focus) next)
         (focus next)))

     (defun handle-navigation-enter (el event)
       (request "selection" (create :element (@ (@ document active-element) id))))

     (defvar *last-focus* nil)

     (defun clear-focus (el)
       (setf (@ el style background-color) (@ el saved-background)
             *last-focus* nil))

     (defun handle-focus (el event)
       (when *last-focus* (clear-focus *last-focus*))
       (let ((child (@ el first-child)))
         (setf (@ child saved-background) (@ child style background-color)
               (@ child style background-color) "darkblue"
               *last-focus* child)))

     (defun handle-blur (el event)
       (let ((child (@ el first-child)))
         (clear-focus child))))))

(defun time-js-file () *time-js-file*)
