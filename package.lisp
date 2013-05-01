
(defpackage time
  (:use common-lisp deck-client iterate cl-who local-time web)
  (:import-from hunchentoot url-encode url-decode session-value escape-for-html)
  (:import-from helpers defun-simple-memoized)
  (:import-from local-time now)
  (:import-from color-gradients make-linear-gradient))

(deck-client:connect-to-deck)
