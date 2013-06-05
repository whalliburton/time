(in-package :time)

(define-showable all (("time:task" (:!= "deleted" t))))
(define-showable tasks (("time:task" (:and (:!= "completed" t) (:!= "deleted" t)))))
(define-showable tags (("time:tag")))
(define-showable completed (("time:task" (:and (:= "completed" t) (:!= "deleted" t)))))
(define-showable deleted (("time:task" (:= "deleted" t))))

(defmethod create-options ((type (eql :task)) node)
  (let ((options
          `(,(if (field-value node "completed")
               '("uncomplete" :onselection uncomplete-task)
               '("complete" :onselection complete-task))
             ,(if (field-value node "deleted")
                '("undelete" :onselection undelete-task)
                '("delete" :onselection delete-task))
             ("tag" :onselection tag-task))))
    (or options '(("no options")))))

(defun selected-task ()
  (iter (for row in (stack-column-elements (nth-stack-column 0)))
        (destructuring-bind (el &key selected &allow-other-keys) row
          (when selected
            (return el)))
        (finally (error "no selected task"))))

(defun complete-task (index name)
  (declare (ignore index name))
  (deck:set-fields (selected-task) `(("completed" t) ("completed on" ,(now))))
  (setup-showing)
  (rerender-body))

(defun uncomplete-task (index name)
  (declare (ignore index name))
  (deck:unset-field (selected-task) "completed")
  (deck:unset-field (selected-task) "completed on")
  (setup-showing)
  (rerender-body))

(defun undelete-task (index name)
  (declare (ignore index name))
  (deck:unset-field (selected-task) "deleted")
  (setup-showing)
  (rerender-body))

(defun delete-task (index name)
  (declare (ignore index name))
  (select-column "options" "delete")
  (stack-push
   '("confirmation"
     (("yes" :onselection finish-delete-task))))
  (rerender-body))

(defun finish-delete-task (index name)
  (declare (ignore index name))
  (deck:set-fields (selected-task) `(("deleted" t)))
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

(defun list-all-tags ()
  (deck:search "time:tags"))

(defun list-task-tags (task)
  (deck:search `(("time:task" (:= :id ,task)) "tagged")))

(defun task-tags-available (task)
  (set-difference (list-all-tags) (list-task-tags task) :key #'id))

(defun tag-task (index name)
  (declare (ignore index name))
  (select-column "options" "tag")
  (stack-push
   `("tags"
     ,(let ((available (task-tags-available (selected-task))))
        (if available
          (iter (for tag in available)
                (collect `(,tag :onselection finish-tag-task)))
          `(("no available tags"))
          ))))
  (rerender-body))

(defun finish-tag-task (index tag)
  (declare (ignore index))
  (deck:add-edge "tagged" (selected-task) tag)
  (setup-showing)
  (rerender-body))