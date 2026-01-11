(in-package :coap)

(defclass client (endpoint) ())

(defmethod client-request ((client client) method host port &optional options payload (type :confirmable))
  (endpoint-send-packet client host port
    (make-packet
      :type type
      :code method
      :token-length 0
      :token 0
      :id 0
      :options options
      :payload payload)))
  
(defun get-request (uri)
  (multiple-value-bind (host port options) (parse-coap-uri uri)
    (let ((client (make-instance 'client :ip nil :port nil)))
      (client-request client :get host port options))))
; TODO wait for a response here

;(get-request "coap://localhost/time")
