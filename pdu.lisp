; Low-level serialization and deserialization of CoAP packets
; important functions:

(in-package :coap)

(defconstant +version-field+ 1)

(defstruct pdu
  ; message type. :con :non :ack :rst
  (type :con :type (member :con :non :ack :rst))
  ; message request/response code. Eg :get or :not-found
  (code :get :type (or symbol integer))
  ; Length of token field
  (token-length 0 :type integer)
  ; Value of token field
  (token 0 :type integer)
  ; Message ID
  (id 0 :type integer)
  ; List of struct option options
  (options nil :type list)
  ; Optional payload
  (payload nil :type (or null string (array (unsigned-byte 8)))))

(defstruct option
  ; Option type eg :uri-path or integer
  (type :uri-path :type (or symbol integer))
  ; Option value
  (value 0 :type (or integer string (array unsigned-byte 8)))
  ; On-the-wire length of option value. This must match (length (option-value
  ; value)) unless the value is an integer, in which case it represents the
  ; byte width of the int.
  (serialized-length 1 :type integer))

(defun make-string-option (type value)
  (make-option :type type :value value :serialized-length (length value)))

(defenumfield message-code
  (:get 001)
  (:post 002)
  (:put 003)
  (:delete 004)
  (:created 201)
  (:deleted 202)
  (:valid 203)
  (:changed 204)
  (:content 205)
  (:bad-request 400)
  (:unauthorized 401)
  (:bad-option 402)
  (:forbidden 403)
  (:not-found 404)
  (:method-not-allowed 405)
  (:not-acceptable 406)
  (:precondition-failed 412)
  (:request-entity-too-large 413)
  (:unsupported-content-format 415)
  (:internal-server-error 500)
  (:not-implemented 501)
  (:bad-gateway 502)
  (:service-unavailable 503)
  (:gateway-timeout 504)
  (:proxying-not-supported 505))

(defun make-message-code (class detail) (+ (* 100 class) detail))
(defun decompose-message-code (code) (floor code 100))

(defenumfield message-type
  (:con 0)
  (:non 1)
  (:ack 2)
  (:rst 3))

(defenumfield option-type
  (:if-match 1)
  (:uri-host 3)
  (:etag 4)
  (:if-none-match 5)
  (:uri-port 7)
  (:location-path 8)
  (:uri-path 11)
  (:content-format 12)
  (:max-age 14)
  (:uri-query 15)
  (:accept 17)
  (:location-query 20)
  (:proxy-uri 35)
  (:proxy-scheme 39)
  (:size1 60)
  (:request-tag 292))

(defparameter *option-formats*
 '((:if-match . :opaque)
   (:uri-host . :string)
   (:etag . :opaque)
   (:if-none-match . :empty)
   (:uri-port . :uint)
   (:location-path . :string)
   (:uri-path . :string)
   (:content-format . :uint)
   (:max-age . :uint)
   (:uri-query . :string)
   (:accept . :uint)
   (:location-query . :string)
   (:proxy-uri . :string)
   (:proxy-scheme . :string)
   (:size1 . :uint)
   (:request-tag . :opaque)))

(defun be32->int (bytes)
  (labels ((shift-start (bytes) (* (1- (length bytes)) 8)))
    (etypecase bytes
      (list (loop
              :for b :in bytes
              :for shift :from (shift-start bytes) :downto 0 :by 8
              :sum (ash b shift)))
      (array (loop
               :for b :across bytes
               :for shift :from (shift-start bytes) :downto 0 :by 8
               :sum (ash b shift))))))

(defun deserialize-coap-pdu (bytes)
  "deserialize coap packet from byte array bytes"
  (let*
      ((stream (byte-stream:make-bytes-input-stream bytes))
       (hdr (loop :for i :from 0 :below 4 :collect (read-byte stream)))
       (version (ldb (byte 2 6) (first hdr)))
       (type (ldb (byte 2 4) (first hdr)))
       (token-length (ldb (byte 4 0) (first hdr)))
       (code-class (ldb (byte 3 5) (second hdr)))
       (code-detail (ldb (byte 5 0) (second hdr)))
       (message-id (logior (ash (third hdr) 8) (fourth hdr)))
       (token (be32->int (loop :for i :from 0 :below token-length :collect (read-byte stream))))
       (option-number 0)
       (options
         (loop
           :for (opt new-option-number) = (multiple-value-list (deserialize-option stream option-number))
           :while opt
           :do (setf option-number new-option-number)
           :collect opt)))
    (let ((end-of-header (read-byte stream nil nil)))
      (assert (or (not end-of-header) (= end-of-header #xff))))
    (make-pdu
      :type (deserialize-or-passthrough-message-type type)
      :token-length token-length
      :token token
      :code (deserialize-or-passthrough-message-code (make-message-code code-class code-detail))
      :id message-id
      :options options
      :payload (byte-stream:read-bytes stream nil))))

;   Option Length:  4-bit unsigned integer.  A value between 0 and 12
;    indicates the length of the Option Value, in bytes.  Three values
;    are reserved for special constructs:
;    13:  An 8-bit unsigned integer precedes the Option Value and
;       indicates the Option Length minus 13.
;    14:  A 16-bit unsigned integer in network byte order precedes the
;       Option Value and indicates the Option Length minus 269.
;    15:  Reserved for future use.  If the field is set to this value,
;       it MUST be processed as a message format error.
(defun read-varlen-field (stream value-or-varlen)
  (case value-or-varlen
    (13 (+ (read-byte stream) 13))
    (14 (+ (logior (ash (read-byte stream) 8) (read-byte stream)) 269))
    (15 (error "invalid value for varlen field header"))
    (otherwise value-or-varlen)))

(defun deserialize-option-value (option-type arr)
  (case (cdr (assoc option-type *option-formats*))
    (:uint (be32->int arr))
    (:string (map 'string #'code-char arr))
    (:opaque arr)
    (otherwise arr)))

(defun deserialize-option (stream current-option-number)
  "deserialize one option from the stream, returning (values option new-option-delta).
   If an end-of-header byte (FF) is encountered, or end of stream, return single nil"
  (let ((peeked-header-byte (byte-stream:peek-byte stream nil nil)))
    (and
      peeked-header-byte
      (/= #xff peeked-header-byte)
      (let* ((delta-length-header (read-byte stream))
             (option-delta (read-varlen-field stream (ldb (byte 4 4) delta-length-header)))
             (option-length (read-varlen-field stream (ldb (byte 4 0) delta-length-header)))
             (new-option-number (+ current-option-number option-delta))
             (option-type (or (deserialize-option-type new-option-number) new-option-number)))
        (values
           (make-option
            :type option-type
            :value (deserialize-option-value option-type (byte-stream:read-bytes stream option-length))
            :serialized-length option-length)
           new-option-number)))))

(defun write-varwidth-int (stream int width)
  (loop
    :for shift :from (* (1- width) 8) :downto 0 :by 8
    :for byte = (logand #xff (ash int (- shift)))
    :do (write-byte byte stream)))

(defun serialize-option-value (stream option)
  (etypecase (option-value option)
    (integer
      (write-varwidth-int stream (option-value option) (or (option-serialized-length option) 4)))
    (string
      (loop :for char :across (option-value option)
            :do (write-byte (char-code char) stream)))
    ((array (unsigned-byte 8))
     (loop :for byte :across (option-value option)
           :do (write-byte byte stream)))))

(defun encode-varlen-field (value)
  "return (values encoded-field extended-bytes-list)"
  (let ((extended-bytes (list)))
    (values
      (cond
        ((>= value 269)
         (prog1 14
             (let ((encoded (- value 269)))
               (push (ash encoded -8) extended-bytes)
               (push (logand encoded #xff) extended-bytes))))
        ((>= value 13)
         (prog1 13
             (let ((encoded (- value 13)))
               (push encoded extended-bytes))))
        (t value))
     (nreverse extended-bytes))))

(defun serialize-option (stream current-option-number option)
  "serialize an option to the stream with header byte, optional extended delta/length, and option payload"
  (let* ((option-type-number (serialize-or-passthrough-option-type (option-type option)))
         (unencoded-delta (- option-type-number current-option-number)))
    (when (minusp unencoded-delta)
      (error "Option delta ~d (from ~d to ~d) is not increasing" unencoded-delta current-option-number option-type-number))
    (multiple-value-bind (delta delta-extended) (encode-varlen-field unencoded-delta)
      (multiple-value-bind (length length-extended) (encode-varlen-field (option-serialized-length option))
        (write-byte (logior (ash delta 4) length) stream)
        (loop :for b :in delta-extended :do (write-byte b stream))
        (loop :for b :in length-extended :do (write-byte b stream))
        (serialize-option-value stream option)))))

(defun serialize-coap-pdu (pdu)
  "serialize a struct pdu to a byte array"
  (sort (pdu-options pdu) #'<
        :key (lambda (option) (serialize-or-passthrough-option-type (option-type option))))
  (let ((stream (byte-stream:make-bytes-output-stream))
        (hdr (list 0 0 0 0)))
    (setf (elt hdr 0) (dpb +version-field+ (byte 2 6) (elt hdr 0)))
    (setf (elt hdr 0) (dpb (serialize-or-passthrough-message-type (pdu-type pdu)) (byte 2 4) (elt hdr 0)))
    (setf (elt hdr 0) (dpb (pdu-token-length pdu) (byte 4 0) (elt hdr 0)))
    (multiple-value-bind (class detail) (decompose-message-code (serialize-or-passthrough-message-code (pdu-code pdu)))
      (setf (elt hdr 1) (dpb class (byte 3 5) (elt hdr 1)))
      (setf (elt hdr 1) (dpb detail (byte 5 0) (elt hdr 1))))
    (setf (elt hdr 2) (ash (logand #xff00 (pdu-id pdu)) -8))
    (setf (elt hdr 3) (logand #xff (pdu-id pdu)))
    (loop :for b :in hdr :do (write-byte b stream))
    (write-varwidth-int stream (pdu-token pdu) (pdu-token-length pdu))
    (loop
      :with option-number = 0
      :for option :in (pdu-options pdu)
      :do (serialize-option stream option-number option)
      :do (setf option-number (serialize-or-passthrough-option-type (option-type option))))
    (when (and (pdu-payload pdu) (plusp (length (pdu-payload pdu))))
      (write-byte #xff stream)
      (etypecase (pdu-payload pdu)
        (string
         (loop :for char :across (pdu-payload pdu) :do (write-byte (char-code char) stream)))
        ((array (unsigned-byte 8))
         (loop :for b :across (pdu-payload pdu) :do (write-byte b stream)))))
    (byte-stream:stream-buffer stream)))
