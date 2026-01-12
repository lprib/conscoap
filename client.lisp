(in-package :coap)

(defclass client (endpoint) ())

(defparameter *token-counter* 0)
(defparameter *id-counter* 0)

(defun gen-request-packet (&key method options payload (type :confirmable))
  (make-packet
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
           (request-packet (gen-request-packet :method :get :options options :payload nil))
           (response-spec (make-instance 'message-spec
                                         :id (packet-id request-packet)
                                         :token (packet-token request-packet)
                                         ;:host host ;TODO how to compare hosts with dns etc
                                         :port port)))
      (endpoint-send-packet client host port request-packet)
      ; TODO for now, always assume text/plain. need to check Content-Type
      (map 'string #'code-char
           (packet-payload (endpoint-wait-for-packet client response-spec))))))

; (get-request "coap://localhost/time")
