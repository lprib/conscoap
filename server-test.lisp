;(defpackage :coap-server-test
;  (:use
;    :cl
;    :coap)

;(in-package :coap-server-test)
(in-package :coap)

(defparameter *server*
  (make-instance 'server
    :ip #(0 0 0 0)
    :port 5683))

(server-add-resource *server*
 (make-resource "/thing/test"
  :get-handler (lambda (req) (declare (ignore req)) (make-response :content "Hello"))
  :post-handler (lambda (req) (declare (ignore req)) (make-response :changed "posted up"))))

(server-listen-once *server*)

