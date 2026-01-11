(in-package :coap)

(defparameter *server* (usocket:socket-connect nil nil :protocol :datagram :element-type '(unsigned-byte 8) :local-host "0.0.0.0" :local-port 5683))

(defun hex (arr)
 (loop :for b :across arr :do (format t "~x " b))
 (format t "~%"))

(multiple-value-bind (buf len host port) (usocket:socket-receive *server* nil 4096)
  (declare (ignore host port))
  (let* ((pkt (make-array len :element-type '(unsigned-byte 8) :displaced-to buf))
         (des (deserialize-coap-packet pkt)))
    (rx-packet des)))

; TODO move all this to non server specific
(defun split-path (path)
  (delete-if (lambda (p) (zerop (length p)))
             (uiop:split-string path :separator "/")))

(defun resource-path-from-packet (packet)
  (loop
    :for option :in (packet-options packet)
    :when (eq (option-type option) :uri-path)
    :collect (option-value option)))

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

(defclass server ()
  ((ip :initarg :ip :initform #(0 0 0 0))
   (port :initarg :port :initform 5683)
   (socket :initform nil :accessor server-socket)
   (handlers :initform (make-hash-table :test 'equal) :accessor server-handlers)))

(defmethod initialize-instance :after ((self server) &key &allow-other-keys)
  (setf (slot-value self 'socket)
        (usocket:socket-connect
          nil nil
          :protocol :datagram
          :element-type '(unsigned-byte 8)
          :local-host (slot-value self 'ip)
          :local-port (slot-value self 'port))))

(defmethod listen-once ((server server))
  (multiple-value-bind (buf len src-host src-port) (usocket:socket-receive (server-socket server) nil 4096)
    (let* ((bytes (make-array len :element-type '(unsigned-byte 8) :displaced-to buf))
           (packet (deserialize-coap-packet bytes))
           (path (resource-path-from-packet packet))
           (handler (or (gethash path (server-handlers server)) #'default-404-handler)))
      ; for now treat handlers as (lambda (packet) response)
      (let* ((response (funcall handler packet))
             (response-packet (construct-matching-response-packet packet response))
             (serialized-response (serialize-coap-packet response-packet)))
        (usocket:socket-send
          (server-socket server)
          serialized-response
          (length serialized-response)
          :host src-host
          :port src-port)))))


(defmethod register-handler ((server server) path handler)
  (setf (gethash (split-path path) (server-handlers server))
        handler))

(defun construct-matching-response-packet (request-packet response)
  "Make a matching response packet form the request packet.
  CON->ACK
  NON->NON
  matching tokens and IDs"
  (make-packet
    :type (ecase (packet-type request-packet)
            (:confirmable :acknowledgement)
            (:non-confirmable :non-confirmable))
    :code (response-code response)
    :token-length (packet-token-length request-packet)
    :token (packet-token request-packet)
    :id (packet-id request-packet)
    :payload (response-payload response)))

(defun default-404-handler (request)
  (declare (ignore request))
  (make-response :not-found))

(defparameter *server* (make-instance 'server :port 8888))
(listen-once *server*)

(defun my-handler (request)
  (declare (ignore request))
  (make-response :content "hello there"))

(register-handler *server* "a/b" #'my-handler)
