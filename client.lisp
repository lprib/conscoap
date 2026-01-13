(in-package :coap)

(defclass client (endpoint) ())

(defparameter *token-counter* 0)
(defparameter *id-counter* 0)

(defun generate-request-pdu (&key method options payload (type :con))
  (make-pdu
    :type type
    :code method
    :token-length 1
    :token (prog1 *token-counter* (incf *token-counter*))
    :id (prog1 *id-counter* (incf *id-counter*))
    :options options
    :payload payload))
  
(defun get-request (uri)
  (multiple-value-bind (host port options) (parse-coap-uri uri)
    (let* ((client (make-instance 'client :ip nil :port nil))
           (request-pdu (generate-request-pdu :method :get :options options :payload nil))
           (response-spec (make-instance 'message-spec
                                         :id (pdu-id request-pdu)
                                         :token (pdu-token request-pdu)
                                         ;:host host ;TODO how to compare hosts with dns etc
                                         :port port)))
      (endpoint-send-pdu client host port request-pdu)
      ; TODO for now, always assume text/plain. need to check Content-Type
      (map 'string #'code-char
           (pdu-payload (endpoint-wait-for-pdu client response-spec))))))

;(get-request "coap://localhost/example_data")
