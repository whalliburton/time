(in-package :time)

(define-command tasks (argument)
  (declare (ignore argument))
  (setf (session-value 'tasks) (deck:search "time:task")
        (session-value 'selected-task) nil)
  (set-stack
   'tasks
   '(("tasks" ("first" "second" "third"))
     ("options" ("complete" "delete"))
     ("confirm" ("yes" "no"))))
  (view-page 'stack))



