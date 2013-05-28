(in-package :time)

(defun start-session ()
  (setf *deck-id* (start-deck-session "time" "93B5C07A585C94B1" :possibly-create-user t))
  (start-printer-session))

(defparameter *templates*
  '(("task" (("title" :string) ("priority" :integer) ("deleted" :boolean)
             ("completed" :boolean) ("completed on" :date)))
    ("tag" (("name" :string)))
    ("tagged" "task" "tag" "task of")))

(defun build (&key recreate)
  (create-templates *templates* recreate))

(defmethod sail:serialize-replacement ((fields-base fields-base))
  (id fields-base))

