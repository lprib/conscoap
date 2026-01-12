(in-package :coap)

(defconstant +pkt-buffer-len+ 4096)
(defparameter +coap-uri-prefix+ "coap://")
(defconstant +default-port+ 5683)

(defclass message-spec ()
  ((id :initarg :id :reader message-spec-id)
   (token :initarg :token :initform nil :reader message-spec-token)
   (type :initarg :type :initform nil :reader message-spec-type)
   (host :initarg :host :initform nil :reader message-spec-host)
   (port :initarg :port :initform nil :reader message-spec-port)))

(defun matches-spec-p (spec packet &optional src-host src-port)
  (with-accessors
    ((spec-id message-spec-id)
     (spec-token message-spec-token)
     (spec-type message-spec-type)
     (spec-host message-spec-host)
     (spec-port message-spec-port))
    spec
    (and
      (or (not spec-id) (equal spec-id (packet-id packet)))
      (or (not spec-token) (equal spec-token (packet-token packet)))
      (or (not spec-type) (equal spec-type (packet-type packet)))
      (or (not spec-host) (not src-host) (equal spec-host src-host))
      (or (not spec-port) (not src-port) (equal spec-port src-port)))))

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

(defmethod endpoint-wait-for-any-packet ((endpoint endpoint))
  (multiple-value-bind
      (buf len host port)
      (usocket:socket-receive (endpoint-socket endpoint) nil +pkt-buffer-len+)
    (let* ((bytes (make-array len :element-type '(unsigned-byte 8) :displaced-to buf)))
       (values host port (deserialize-coap-packet bytes)))))


(defmethod endpoint-wait-for-packet ((endpoint endpoint) message-spec)
  "wait for a single packet on the endpoint. returns (values host port deserialized-packet)"
  (loop
    :for (host port packet) = (multiple-value-list (endpoint-wait-for-any-packet endpoint))
    :when (matches-spec-p message-spec packet host port)
      :return packet))

(defun split-path (path)
  (delete-if (lambda (p) (zerop (length p))
               (uiop:split-string path :separator "/"))))

(defun split-on-first (string char)
  (let ((pos (position char string)))
    (if pos
        (values (subseq string 0 pos)
              (subseq string (1+ pos)))
        (values string ""))))

(defun parse-coap-uri (uri)
  "returns (values host port options)
  options includes all the URI options required for this packet (host port path query)"
  (unless (uiop:string-prefix-p +coap-uri-prefix+ uri)
    (error "URI must begin with ~a" +coap-uri-prefix+))
  ; oh god oh fuck why is destructuring so shit in CL
  (let* ((without-prefix (subseq uri (length +coap-uri-prefix+))))
    (multiple-value-bind (host-port path-opts) (split-on-first without-prefix #\/)
      (multiple-value-bind (host port) (split-on-first host-port #\:)
        (multiple-value-bind (whole-path whole-query) (split-on-first path-opts #\?)
          (let ((queries (uiop:split-string whole-query :separator "&"))
                (path-parts (uiop:split-string whole-path :separator "/"))
                (port (if (zerop (length port)) +default-port+ (parse-integer port))))
            (values host port
              (append
                (list
                  (make-string-option :uri-host host)
                  (make-option :type :uri-port :value port :serialized-length 2))
                (loop :for path-part :in path-parts :collect
                      (make-string-option :uri-path path-part))
                (loop :for query :in queries :collect
                      (make-string-option :uri-query query))))))))))

(defun resource-path-from-packet (packet)
  (loop
    :for option :in (packet-options packet)
    :when (eq (option-type option) :uri-path)
    :collect (option-value option)))
