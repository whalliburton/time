(in-package :time)

(defun create-color-gradient (from to &key hex (steps 21))
  (let ((vector
          (second
           (multiple-value-list
            (make-linear-gradient '(0 0) '(0 10) :steps steps
                                                 :color-1 (multiple-value-list (rgba-from-hex (color-hex from)))
                                                 :color-2 (multiple-value-list (rgba-from-hex (color-hex to))))))))
    (if hex
      (iter (for (r g b a) in-vector vector)
            (collect (rgb->web r g b)))
      vector)))

(defvar *scripts* nil)

(defmacro script (text)
  `(if *scripts*
     (push ,text *scripts*)
     (with-html-output (stream)
       (htm (:script :type "text/javascript" (str ,text))))))

(defun render-time-front-page (stream)
  (let ((page (session-value 'page)))
    (with-html-output (stream)
      (:div (:input :style "width:600px;"
                    :type "text" :id "command" :onkeypress "sendOnEnter(this,event,\"command\");"))
      (cond
        ((eq page 'render-page-error) (htm (:div :style "padding:4px;" (esc (session-value 'error-message)))))
        (page (funcall page stream))
        (t (htm (:div :style "padding:20px" (str "Teach me. 'Help' is available.")))))
      (if (session-value 'select)
        (script (format nil "moveTo('~A');" (session-value 'select)))
        (script "focusId('command');")))))

(defun rerender-body ()
  (let* ((*scripts* (list 'scripts))
         (body (with-output-to-string (stream) (render-time-front-page stream)))
         (scripts (cdr (reverse *scripts*))))
    (format nil "setBody(\"~A\");~{~A~}" (url-encode body) scripts)))

(defun set-stack (key stack)
  (setf (session-value 'stack) (cons key stack)))

(defparameter *gradient* (let* ((steps 5)
                                (list
                                  (nconc
                                   (create-color-gradient "red" "orange" :hex t :steps steps)
                                   (create-color-gradient "orange" "yellow" :hex t :steps steps)
                                   (create-color-gradient "yellow" "green" :hex t :steps steps)
                                   (create-color-gradient "green" "blue" :hex t :steps steps)
                                   (create-color-gradient "blue" "violet" :hex t :steps steps)
                                   )))
                           (make-array (length list) :initial-contents list)))

(defun render-stack (stream)
  (let (scripts
        (gradient-index 0)
        (stack (session-value 'stack)))
    (flet ((next-gradient ()
             (prog1
                 (aref *gradient* gradient-index)
               (incf gradient-index))))
      (script (format nil "setShortcutMove('command','down','~A');" (caadr (session-value 'stack))))
      (with-html-output (stream)
        (:table
         :id "stack"
         (:tr
          (iter
           (with last-name)
           (for el on (cdr stack))
           (for (name rows) in (cdr stack))
           (htm
            (:td :valign :top :style "padding-right:40px;"
                 (:table :id name
                         (iter (for row in rows)
                               (for index from 0)
                               (htm (:tr (:td :tabindex 0
                                              :onfocus "handleFocus(this,event);"
                                              :onblur "handleBlur(this,event);"
                                              :id (format nil "~A-~A" name index)
                                              (:div :class "box"
                                                    :style (format nil "background-color:~A;" (next-gradient))
                                                    (render-stack-element stream (car row)))))))))
            (push (list
                   name name
                   (if last-name (prin1-to-string last-name) "\"command\"")
                   (if (cdr el) (prin1-to-string (caadr el)) "false"))
                  scripts)
            (setf last-name name)))))
        (let ((scripts
                (with-output-to-string (stream)
                  (iter (for script in (nreverse scripts))
                        (apply #'format stream "setupNavigation(~S,\"~A-\",~A,~A);" script)))))
          (script scripts))
        (script "setShortcutFn(\"stack\",81,function () {request(\"pop-stack\");} );")))))

(defun-simple-memoized template-type-keyword (template-id)
  (intern (string-upcase (field-value (deck:get-node template-id) "name")) :keyword))

(defun render-stack-element (stream row)
  (etypecase row
    (string (princ (cl-who:escape-string row) stream))
    (node (render (template-type-keyword (template-id row)) stream row))))

(defmethod render (type stream node)
  (with-html-output (stream)
    (:table
     (:tr
      (:th (str "id"))
      (:td (str (id node))))
     (iter (for (name val) in (fields node))
           (htm (:tr (:th (esc name)) (:td (esc (princ-to-string val)))))))))

(defun handle-selection (element)
  (let* ((pos (position #\- element))
         (category (subseq element 0 pos))
         (index (parse-integer (subseq element (1+ pos))))
         (stack (session-value 'stack))
         (column (second (or (assoc category (cdr stack) :test #'equal)
                             (error "Unknown category ~S." category))))
         (row (nth index column)))
    (destructuring-bind (name &key onselection) row
      (when onselection
        (etypecase onselection
          (symbol (funcall onselection index name)))))))

(defun initialize-stack (key new-stack)
  (let ((stack (session-value 'stack)))
    (if (or (null stack) (not (equal key (car stack))))
      (set-stack key new-stack))))

(defun stack-push (column)
  (set-stack
   (car (session-value 'stack))
   (append (cdr (session-value 'stack)) column))
  (setf (session-value 'select) (caar column)))

(defun stack-pushnew (column)
  (unless (assoc (caar column) (cdr (session-value 'stack)) :test 'equal)
    (stack-push column)))

(defun possibly-remove-column (name)
  (set-stack
   (car (session-value 'stack))
   (iter (for column in (cdr (session-value 'stack)))
         (unless (equal name (car column))
           (collect column)))))

(defmacro define-page (name &body body)
  (destructuring-bind (name &key no-heading) (ensure-list name)
    `(defun ,(symb 'render-page- name) (stream)
       (with-html-output (stream)
         (:div :class "page"
               ,@(unless no-heading `((:h1 (str ,(string-downcase name)))))
               ,@body)))))

(define-page (stack :no-heading t)
  (:h1 (esc (string-downcase (car (session-value 'stack)))))
  (render-stack stream))

(defun pop-stack ()
  (setf (session-value 'stack) (butlast (session-value 'stack)))
  (when (null (cdr (session-value 'stack)))
    (setf (session-value 'page) nil))
  (setf (session-value 'select) (car (last1 (cdr (session-value 'stack)))))
  (rerender-body))