(in-package :coap)

(defconstant +pkt-buffer-len+ 4096)

(defclass endpoint ()
  ((ip :initarg :ip :initform #(0 0 0 0))
   (port :initarg :port :initform 5683)
   (socket :initform nil :accessor endpoint-socket)))

(defmethod initialize-instance :after ((self endpoint) &key &allow-other-keys)
  (setf (slot-value self 'socket)
        (usocket:socket-connect
          nil nil
          :protocol :datagram
          :element-type '(unsigned-byte 8)
          :local-host (slot-value self 'ip)
          :local-port (slot-value self 'port))))

(defmethod endpoint-send-packet ((endpoint endpoint) host port packet)
  (let ((serialized-packet (serialize-coap-packet packet)))
    (usocket:socket-send
      (endpoint-socket endpoint)
      serialized-packet
      (length serialized-packet)
      :host host
      :port port)))

(defmethod endpoint-wait-for-packet ((endpoint endpoint))
  "wait for a single packet on the endpoint. returns (values host port deserialized-packet)"
  (multiple-value-bind
      (buf len host port)
      (usocket:socket-receive (endpoint-socket endpoint) nil +pkt-buffer-len+)
    (let* ((bytes (make-array len :element-type '(unsigned-byte 8) :displaced-to buf)))
       (values host port (deserialize-coap-packet bytes)))))
