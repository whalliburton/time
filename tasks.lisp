(in-package :time)

(define-showable tasks (("time:task" (:and (:!= "completed" t) (:!= "deleted" t)))))
(define-showable completed (("time:task" (:and (:= "completed" t) (:!= "deleted" t)))))
(define-showable deleted (("time:task" (:= "deleted" t))))

(defmethod create-options ((type (eql :task)) node)
  (let ((options
          `(,@(unless (field-value node "completed")
                '(("complete" :onselection complete-task)))
              ,@(unless (field-value node "deleted")
                  '(("delete" :onselection delete-task))))))
    (or options
        '(("no options")))))

(defun complete-task (index name)
  (declare (ignore index name))
  (deck:set-fields (session-value 'selected) `(("completed" t) ("completed on" ,(now))))
  (setup-showing)
  (rerender-body))

(defun delete-task (index name)
  (declare (ignore index name))
  (deck:set-fields (session-value 'selected) `(("deleted" t)))
  (setup-showing)
  (rerender-body))

(defmethod render ((type (eql :task)) stream node)
  (with-html-output (stream)
    (when (field-value node "deleted") (web::icon :trash))
    (esc (field-value node "title"))))