; Implements a CoAP server. Users can register handlers on the server to be
; called when specific resources are requested

(in-package :coap)

; A resource is a path that the server will respond to
(defclass resource ()
  ((path
     :initarg :path
     :reader resource-path
     :documentation "single string path")
   (path-parts
     :initform nil
     :reader resource-path-parts
     :documentation "list of strings for each path part")
   (handlers
     :initarg :handlers
     :accessor resource-handlers
     :initform '(:get nil :post nil :put nil :delete nil)
     :documentation "handlers for various operations. Each handler takes
     (server req) and returns either a string or a response class")
   (attributes
     :initarg :attributes
     :initform nil
     :accessor resource-attributes
     :documentation "attributes to add to .well-known/core. Alist of (string . string)")))

(defmethod initialize-instance :after ((instance resource) &key path &allow-other-keys)
 (setf (slot-value instance 'path-parts) (split-path path)))

(defun make-resource (path &key (attributes nil) (get-handler nil) (post-handler nil) (put-handler nil) (delete-handler nil))
  "Make a coap resource.
    Path is string.
    handlers are of the form (lambda (pdu) response),
      where pdu is struct pdu and response is class response"
  (make-instance 'resource
    :path path
    :handlers (list :get get-handler :post post-handler :put put-handler :delete delete-handler)
    :attributes attributes))

(defmethod resource-add-handler ((resource resource) method handler)
  "Add a handler for the given method to a pre-existing resource. nil to remove handler"
  (unless (member method '(:get :post :put :delete))
    (error "unknown handler method ~a" method))
  (setf (getf (resource-handlers resource) method) handler))

(defmethod resource-add-attribute ((resource resource) key value)
  (setf (resource-attributes resource) (acons key value (resource-attributes resource))))

(defun make-simple-response-handler (code)
  (lambda (request)
    (declare (ignore request))
    (make-response code)))

(defparameter *404-resource*
  (let ((return-404 (make-simple-response-handler :not-found)))
    (make-resource
      ""
      :get-handler return-404
      :post-handler return-404
      :put-handler return-404
      :delete-handler return-404)))

(defparameter *405-handler* (make-simple-response-handler :method-not-allowed))

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
   ((resources :initform (make-hash-table :test 'equal) :accessor server-resources)))

(defmethod server-listen-once ((server server))
  "Listen for a single packet and respond"
  (multiple-value-bind (client-host client-port request-pdu) (endpoint-wait-for-any-pdu server)
    (let* ((path-parts (resource-path-from-pdu request-pdu))
           (resource (or (gethash path-parts (server-resources server)) *404-resource*))
           (handler (or (getf (resource-handlers resource) (pdu-code request-pdu)) *405-handler*))
           (response (funcall handler request-pdu))
           (response-pdu (construct-matching-response-pdu request-pdu response)))
      (endpoint-send-pdu server client-host client-port response-pdu))))

(defmethod server-add-resource ((server server) resource)
  "Add resource to server's resource table"
  (setf (gethash (resource-path-parts resource) (server-resources server)) resource))

(defmethod server-get-resource ((server server) path)
  "Get resource via string path"
  (gethash (split-path path) (server-resources server)))

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
