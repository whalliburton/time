(in-package :time)

(helpers:add-config-parameters
 deck-local-socket-name             "/mnt/projects/sockets/deck"
 deck-cache-invalidation-port       2003
 deck-uses-sharder                  nil)

(defun initialize ()
  (setf hunchentoot:*catch-errors-p* nil)
  (start-sail)
  (start-session)
  (build)
  (start-time-application)
  (format t "Welcome to Time!~%"))

(defmethod hunchentoot:maybe-invoke-debugger ((condition usocket:timeout-error))
  (warn "Timeout error. ~S" condition))
