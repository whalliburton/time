(in-package :time)

(helpers:add-config-parameters
 time-rpc-port                      200000
 time-host                          "ec2-local-ip")

(defun start-sail ()
  (sail:start-sail-server
   "time"
   :port (helpers:config time-rpc-port nil)
   :host (helpers:host-or-local-ip time-host)
   :public '()
   :private ()))

(defun restart-sail ()
  (when *sails* (stop-sails))
  (sleep 0.25) ;; to get back the port
  (start-sail))
