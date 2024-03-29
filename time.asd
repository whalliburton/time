(defsystem :time
  :serial t
  :components ((:static-file "time.asd")
               (:file "package")
               (:file "rpc-sail")
               (:file "utility")
               (:file "build")
               (:file "js")
               (:file "render")
               (:file "commands")
               (:file "help")
               (:file "show")
               (:file "now")
               (:file "tasks")
               (:file "application")
               (:file "initialize"))
  :depends-on (:deck-client :web :cl-who :local-time :color-gradients :alexandria))
