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
  :attributes (list (cons "rt" "thingy"))
  :get-handler (lambda (req) (declare (ignore req)) (coap:make-response :content "Hello"))
  :post-handler (lambda (req) (declare (ignore req)) (coap:make-response :changed "posted up"))))

*server*

(server-get-resource *server* "/thing/test")
(resource-add-attribute (server-get-resource *server* "/thing/test") "attrib" "value")

(coap:server-listen-once *server*)

