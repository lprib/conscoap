;(defpackage :coap-server-test
;  (:use
;    :cl
;    :coap)

;(in-package :coap-server-test)
(in-package :coap)

(defparameter *server*
  (make-instance 'coap:server
    :ip #(0 0 0 0)
    :port 5683))

(coap:server-add-resource *server*
 (coap:make-resource "/thing/test"
  :get-handler (lambda (req) (declare (ignore req)) (coap:make-response :content "Hello"))
  :post-handler (lambda (req) (declare (ignore req)) (mcoap:ake-response :changed "posted up"))))

(scoap:erver-listen-once *server*)

