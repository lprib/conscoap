; Implements a CoAP server. Users can register handlers on the server to be
; called when specific resources are requested

(in-package :coap)

(defclass response ()
  ((code
     :initarg :code
     :accessor response-code
     :documentation "the symbol of the response code eg :content")
   (payload
     :initarg :payload
     :initform nil
     :accessor response-payload
     :documentation "the payload, can be nil, string, or byte array")))

(defun make-response (code &optional (payload nil))
  (make-instance 'response :code code :payload payload))

(defclass server (endpoint)
   ((handlers :initform (make-hash-table :test 'equal) :accessor server-handlers)))

(defmethod server-listen-once ((server server))
  "Block waiting for a single packet, and run relevent handler"
  (multiple-value-bind (client-host client-port request-pdu) (endpoint-wait-for-pdu server)
    (let* ((path (resource-path-from-pdu request-pdu))
           (handler (or (gethash path (server-handlers server)) #'default-404-handler))
           (response (funcall handler request-pdu))
           (response-pdu (construct-matching-response-pdu request-pdu response)))
      (endpoint-send-pdu server client-host client-port response-pdu))))

(defmethod server-register-handler ((server server) path handler)
  "Register a handler method on a server for a given resource path
  path should be a string of the form a/b/c
  handler should be a function of the form (lambda (request) response) where
    request is an instance of pdu
    response is an instance of response

  Eg. (server-register-handler *server* 'a/b'
    (lambda (req) (make-response :content 'hi')))
  "
  (setf (gethash (split-path path) (server-handlers server))
        handler))

(defun construct-matching-response-pdu (request-pdu response)
  "Make a matching response pdu form the request pdu.
  CON->ACK
  NON->NON
  matching tokens and IDs"
  (make-pdu
    :type (ecase (pdu-type request-pdu)
            (:con :ack)
            (:non :non))
    :code (response-code response)
    :token-length (pdu-token-length request-pdu)
    :token (pdu-token request-pdu)
    :id (pdu-id request-pdu)
    :payload (response-payload response)))

(defun default-404-handler (request)
  (declare (ignore request))
  (make-response :not-found))
