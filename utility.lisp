(in-package :time)

(defun asdf-base-path (name)
  (directory-namestring (asdf:component-pathname (asdf:find-system name))))

(defun time-file (base)
  (concatenate 'string (asdf-base-path :time) base))

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (when a (princ a s)))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defun last1 (list)
  (car (last list)))

(flet ((generate-control-string (vars)
         (with-output-to-string (s)
           (loop for var on vars
                 do (prin1 (car var) s)
                    (unless (keywordp (car var)) (write-string " ~S" s))
                    (when (cdr var) (format s "  "))))))

  (defmacro bugout (&rest vars)
    "Print VARS, for debugging."
    `(format t ,(with-output-to-string (s)
                  (write-string "~%>>>  " s)
                  (write-string (generate-control-string vars) s)
                  (write-string "~2%" s))
             ,@(remove-if #'keywordp vars)))

  (defmacro breakout (&rest vars)
    "Break with VARS, for debugging."
    `(break ,(generate-control-string vars)
            ,@(remove-if #'keywordp vars))))