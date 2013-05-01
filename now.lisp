(in-package :time)

(define-command now (argument)
  (declare (ignore argument))
  (view-page 'now))

(define-page now
  (setf (session-value 'select) nil)
  (fmt "~A" (format-timestring nil (now) :format +asctime-format+)))




