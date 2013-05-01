(in-package :time)

(defvar *commands* nil)

(defmacro define-command (name args &body body)
  `(progn
     (pushnew ',name *commands*)
     (defun ,(symb 'time-command- name) ,args ,@body)))

(defun split-out-command (string)
  (let ((pos (or (position #\space string) (length string))))
    (values (string-downcase (subseq string 0 pos))
            (string-left-trim '(#\space) (subseq string pos)))))

(defun handle-command (value)
  (multiple-value-bind (command arguments) (split-out-command (url-decode value))
    (cond
      ((member command *commands* :test #'string-equal)
       (funcall
        (intern (mkstr 'time-command- (string-upcase command)) :time) arguments))
      (t (unknown-command command))))
  (rerender-body))

(defun view-page (name) (setf (session-value 'page) (symb 'render-page- name)))
(defun current-page () (session-value 'page))

(defun unknown-command (value)
  (setf (session-value 'error-message)
        (format nil "'~A' was not understood. Perhaps you need 'Help'?" (string-capitalize value)))
  (view-page 'error))

(define-command add (title)
  (deck:add-node "time:task" `(("title" ,title)))
  (time-command-show "tasks"))





