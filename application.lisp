(in-package :time)

(defvar *web-server* nil)

(defun start-time-application ()
  (when *web-server* (web::stop *web-server*))
  (setf *web-server*
        (make-instance 'web:web-server
                       :port 9999
                       :name "time"
                       :title "Time"
                       :renderer 'render-time-front-page
                       :commands '(("command" handle-command :value)
                                   ("selection" handle-selection :element)
                                   ("pop-stack" pop-stack))
                       :dispatches
                       `(,(hunchentoot:create-static-file-dispatcher-and-handler "/time.css" (time-file "time.css"))
                          ,(hunchentoot:create-static-file-dispatcher-and-handler "/shortcut.js" (time-file "shortcut.js"))
                          ,(hunchentoot:create-prefix-dispatcher "/time.js" 'web-js::time-js-file))
                       :headings '("<script src='/time.js' type='text/javascript'></script> "
                                   "<script src='/shortcut.js' type='text/javascript'></script> "
                                   "<link rel=stylesheet type=\"text/css\" href=/time.css />" )
                       :initialize-session nil)))
