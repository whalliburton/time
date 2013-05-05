(in-package :time)

(define-command help (category)
  (setf (session-value 'help-category) category
        (session-value 'select) "categories")
  (set-stack
   'help
   `(("categories"
      (("introduction" :onselection select-help)
       ("tasks" :onselection select-help)
       ("now" :onselection select-help)))))
  (view-page 'stack))

(defun select-help (index category)
  (declare (ignore index))
  (possibly-remove-column "text")
  (stack-push
   `("text"
     ,(cond
        ((equal category "introduction")
         '(("This is the introduction.")))
        ((equal category "tasks")
         '(("'Tasks' shows you all the tasks.")))
        ((equal category "now")
         '(("'Now' informs you about the present moment."))))))
  (select-column "categories" category)
  (rerender-body))

