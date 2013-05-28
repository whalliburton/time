(in-package :time)

(define-showable all (("time:task" (:!= "deleted" t))))
(define-showable tasks (("time:task" (:and (:!= "completed" t) (:!= "deleted" t)))))
(define-showable tags (("time:tag")))
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
    (:span :style "padding-right:10px;"
           (web::icon (if (field-value node "completed") :check :check-empty)))
    (esc (field-value node "title"))))

(defmethod render ((type (eql :tag)) stream node)
  (with-html-output (stream)
    (:span :style "padding-right:10px;" (web::icon :tag))
    (esc (field-value node "name"))))

(define-command add (cmd)
  (multiple-value-bind (what arg) (split-out-command cmd)
    (cond
      ((equal what "task")
       (add-task arg)
       (time-command-show "tasks")))
    (cond
      ((equal what "tag")
       (deck:add-node "time:tag" `(("name" ,arg)))
       (time-command-show "tags")))))

(defun add-task (raw)
  (deck:add-node "time:task" `(("title" ,raw))))
