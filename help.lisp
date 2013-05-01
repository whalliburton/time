(in-package :time)

(define-command help (category)
  (setf (session-value 'help-category) category
        (session-value 'select) "categories")
  (set-stack
   'help
   `(("categories"
      (("introduction" select-help)
       ("tasks" select-help)
       ("now" select-help)))))
  (view-page 'stack))

(defun select-help (category)
  (possibly-remove-column "text")
  (stack-push
   `(("text"
      ,(cond
         ((equal category "introduction")
          '(("This is the introduction.")))
         ((equal category "tasks")
          '(("'Tasks' shows you all the tasks.")))
         ((equal category "now")
          '(("'Now' informs you about the present moment.")))))))
  (rerender-body))




