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

(defmacro stack () `(session-value 'stack))

(defun stack-column-elements (column)
  (second column))

(defun nth-stack-column (n)
  (nth n (cdr (stack))))

(defun set-stack (key stack)
  (setf (stack) (cons key (copy-tree stack))))

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

(defun stack-level-name (level)
  (if (consp level) (car level) level))

(defun render-stack (stream)
  (let (scripts
        (gradient-index 0)
        (stack (stack)))
    (flet ((next-gradient ()
             (prog1
                 (aref *gradient* gradient-index)
               (incf gradient-index))))
      (script (format nil "setShortcutMove('command','down','~A');" (caadr (stack))))
      (with-html-output (stream)
        (:table
         :id "stack"
         (:tr
          (iter
           (with last-name)
           (for el on (cdr stack))
           (for (raw-name rows) in (cdr stack))
           (when rows
             (let ((has-selected
                     (iter (for row in rows)
                           (destructuring-bind (el &key selected &allow-other-keys) row
                             (when selected (return t)))))
                   (name (stack-level-name raw-name)))
               (htm
                (:td :valign :top :style "padding-right:40px;"
                     (:table :id name
                             (iter (for row in rows)
                                   (for index from 0)
                                   (destructuring-bind (el &key selected onenter value &allow-other-keys) row
                                     (htm (:tr (:td :tabindex 0
                                                    :onfocus "handleFocus(this,event);"
                                                    :onblur "handleBlur(this,event);"
                                                    :class (cond (selected "selected")
                                                                 (has-selected "unselected"))
                                                    :id (format nil "~A-~A" name index)
                                                    (:div :class (cond
                                                                   (selected "box selected")
                                                                   (has-selected "box unselected")
                                                                   (t "box"))
                                                          :style (format nil "background-color:~A;" (next-gradient))
                                                          (render-stack-element stream el onenter value)))))))))
                (push (list
                       name name
                       (if last-name "false"
                       ; (prin1-to-string last-name)
                         "\"command\"")
                       (if (cdr el)
                         (prin1-to-string (stack-level-name (caadr el)))
                         "false")
                       (if (equal name "input") "false" "true"))
                      scripts)
                (setf last-name name))))))
        (let ((scripts
                (with-output-to-string (stream)
                  (iter (for script in (nreverse scripts))
                        (apply #'format stream "setupNavigation(~S,\"~A-\",~A,~A,~A);" script)))))
          (script scripts))
        (script "setShortcutFn(\"stack\",32,function () {focus(getById(\"command\"));});")
        )))))

(defun-simple-memoized template-type-keyword (template-id)
  (intern (string-upcase (field-value (deck:get-node template-id) "name")) :keyword))

(defun render-stack-element (stream el onenter value)
  (etypecase el
    (string (princ (cl-who:escape-string el) stream))
    (keyword (ecase el
               (:input (with-html-output (stream)
                         (:input :type "text" :id "input" :value value
                                 :onkeypress (format nil "sendOnEnter(this,event,~S);" onenter))))))
    (node (render (template-type-keyword (template-id el)) stream el))))

(defmethod render (type stream node)
  (with-html-output (stream)
    (:table
     (:tr
      (:th (str "id"))
      (:td (str (id node))))
     (iter (for (name val) in (fields node))
           (htm (:tr (:th (esc name)) (:td (esc (princ-to-string val)))))))))

(defun stack-level-name-compare (a b)
  (equal (stack-level-name a) (stack-level-name b)))

(defun handle-selection (element)
  (let* ((pos (position #\- element))
         (category (subseq element 0 pos))
         (index (parse-integer (subseq element (1+ pos))))
         (stack (stack))
         (column (second (or (assoc category (cdr stack) :test #'stack-level-name-compare)
                             (error "Unknown category ~S." category))))
         (row (nth index column)))
    (destructuring-bind (description &key onselection &allow-other-keys) row
      (when onselection
        (etypecase onselection
          (symbol (funcall onselection index (stack-level-name description))))))))

(defun initialize-stack (key new-stack)
  (let ((stack (stack)))
    (if (or (null stack) (not (equal key (car stack))))
      (set-stack key new-stack))))

(defun column-name (column)
  (if (consp (car column)) (caar column) (car column)))

(defun stack-push (column)
  (set-stack
   (car (stack))
   (append (cdr (stack)) (list column)))
  (setf (session-value 'select) (stack-level-name (car column))))

(defun stack-pushnew (column)
  (unless (assoc (car column) (cdr (stack)) :test 'equal)
    (stack-push column)))

(defun possibly-remove-column (name)
  (set-stack
   (car (stack))
   (iter (for column in (cdr (stack)))
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
  (:h1 (esc (string-downcase (car (stack)))))
  (render-stack stream))

(defun stack-column (name)
  (assoc name (cdr (stack)) :test #'string= :key (lambda (el) (if (consp el) (car el) el))))

(defun unselect-column (column)
  (iter (for row in (second column))
        (remf (cdr row) :selected)))

(defun unselect-column-named (name)
  (unselect-column (stack-column name)))

(defun select-nth-column (name index)
  (unselect-column-named name)
  (let ((column (stack-column name)))
    (iter (for row in (second column))
          (for x from 0)
          (when (= x index)
            (setf (getf (cdr row) :selected) t)))))

(defun select-column (name val)
  (unselect-column-named name)
  (let ((column (stack-column name)))
    (iter (for row in (second column))
          (when (equal (car row) val)
            (setf (getf (cdr row) :selected) t)))))

(defun last-column () (last1 (cdr (stack))))

(defun selected-row-id (column)
  (iter (for row in (second column))
        (for index from 0)
        (when (getf (cdr row) :selected)
          (return (format nil "~A-~A" (column-name column) index)))))

(defun handle-cancel-cleanup ()
  (let ((description (car (last1 (stack)))))
    (when (consp description)
      (destructuring-bind (name &key oncancel &allow-other-keys) description
        (declare (ignore name))
        (when oncancel (funcall oncancel))))))

(defun pop-stack ()
  (handle-cancel-cleanup)
  (setf (stack) (butlast (stack)))
  (when (null (cdr (stack)))
    (setf (session-value 'page) nil
          (session-value 'select) nil))
  (when-let (current (last-column))
    (let ((row-id (selected-row-id current)))
      (unselect-column current)
      (setf (session-value 'select) (or row-id (column-name current)))))
  (rerender-body))

(defun print-stack ()
  (iter (for (name rows) in (cdr (stack)))
        (format t "~A~%" name)
        (iter (for (name . args) in rows)
              (format t "  ~A~40T~@[~S~]~%" name args))))
