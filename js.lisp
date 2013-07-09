(in-package :web-js)

(defparameter *time-js-file*
  (ps*
   '(progn

     (defun set-shortcut-move (target key id)
       (set-shortcut-fn target key (lambda (event) (move-to id))))

     (defun setup-time-navigation (target prefix left right pop)
       (let ((el (get-by-id target)))
         (setup-navigation target prefix)
         (setf (slot-value el 'enter-handler)
               (lambda () (request "selection" (create :element (@ (@ document active-element) id)))))
         (when left (listen el ((@ shortcut build) "left" (lambda (event) (move-to left)))))
         (when right (listen el ((@ shortcut build) "right" (lambda (event) (move-to right)))))
         (when pop
           (set-shortcut-fn target 81 (lambda (event) (request "pop-stack"))))))

     (defun move-to (id)
       (let* ((el (get-by-id id))
              (type (@ el node-name)))
         (if (== type "TABLE")
           (focus (@ (@ (aref el.rows 0) first-child)))
           (focus el))))

     (defun possibly-focus-command (event)
      (unless (is-input (@ event target))
        (focus (get-by-id "command"))))

     )))

(defun time-js-file () *time-js-file*)
