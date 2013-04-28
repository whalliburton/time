(in-package :web-js)

(defparameter *time-js-file*
  (ps*
   '(progn
     (defun setup-navigation (target prefix left right)
      (let* ((el (get-by-id target))
             (opt
               (create :type "keydown"
                       :propagate false
                       :disable_in_input false
                       :target el
                       :keycode false))))
      (setf (slot-value el 'navigation-elements) (collect-children-with-prefix el prefix))
      (when left ((@ shortcut add) "left" (lambda (event) (move-to left)) opt))
      (when right ((@ shortcut add) "right" (lambda (event) (move-to right)) opt))
      ((@ shortcut add) "up" (lambda (event) (handle-navigation el event true)) opt)
      ((@ shortcut add) "down" (lambda (event) (handle-navigation el event false)) opt)
      ((@ shortcut add) "enter" (lambda (event) (handle-navigation-enter el event)) opt))

     (defun move-to (id)
      (let ((el (get-by-id id)))
        (if (slot-value el 'current-selection)
          ((@ (slot-value el 'current-selection) focus))
          (if (slot-value el 'current-focus)
            ((@ (slot-value el 'current-focus) focus))
            ((@ (@ (aref el.rows 0) first-child) focus))))))

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
         ((@ next focus))))

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
               (@ child style background-color) "blue"
               *last-focus* child)))

     (defun handle-blur (el event)
       (let ((child (@ el first-child)))
         (clear-focus child))))))

(defun time-js-file () *time-js-file*)
