(in-package :time)

(defvar *showable* nil)

(defmacro define-showable (name query)
  `(pushnew (list ',name ',query) *showable* :test #'string-equal :key 'car))

(defun setup-showing (&optional selected)
  (let* ((what (session-value 'showing-what))
         (query (second (assoc what *showable* :test #'string-equal)))
         (results (deck:search query)))
    (setf (session-value 'select) "showing"
          (session-value 'showing) results)
    (set-stack
     what
     `(("showing"
        ,(if results
           (iter (for el in results)
                 (collect `(,el :onselection show-options ,@(when (and selected (eql (id el) (id selected)))
                                                              '(:selected t)))))
           `((,(format nil "no ~A" (cl-who:escape-string what))))))))))

(define-command show (what)
  (let ((query (second (assoc what *showable* :test #'string-equal))))
    (unless query
      (setf (session-value 'error-message) (format nil "Don't know how to show '~A'." what))
      (view-page 'error)
      (return-from time-command-show (rerender-body))))
  (setf (session-value 'showing-what) what)
  (setup-showing)
  (view-page 'stack))

(defun show-options (index name)
  (declare (ignore name))
  (let ((selected (nth index (session-value 'showing))))
    (setup-showing selected)
    (stack-push
     `(("options" :oncancel cleanup-options)
       ,(create-options (template-type-keyword (template-id selected)) selected))))
  (rerender-body))

(defun cleanup-options ()
;;  (setf (session-value 'selected) nil)
  )

(defmethod create-options (type node)
  `(("no options")))
