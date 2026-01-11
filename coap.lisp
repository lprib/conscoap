(defpackage :coap
  (:use :cl))

(in-package :coap)

;(defparameter *server* (usocket:socket-connect nil nil :protocol :datagram :element-type '(unsigned-byte 8) :local-host "0.0.0.0" :local-port 5683))

;(multiple-value-bind (buf len host port) (usocket:socket-receive *server* nil 4096)
;  (declare (ignore host port))
;  (let ((pkt (make-array len :element-type '(unsigned-byte 8) :displaced-to buf)))
;    (print pkt)
;    (print (parse-coap-packet pkt))))

(defstruct option
  type
  value
  serialized-length)
 
(defstruct packet
  (version 1)
  type
  code
  token-length
  token
  id
  options
  payload)


(defmacro defenumfield (name &rest variants)
  (let*
      ((alist-name (gensym))
       (upcase-name (string-upcase name))
       (their-package (symbol-package name))
       (serialize-enum (intern (format nil "SERIALIZE-~a" upcase-name) their-package))
       (serialize-or-passthrough-enum (intern (format nil "SERIALIZE-OR-PASSTHROUGH-~a" upcase-name) their-package))
       (deserialize-enum (intern (format nil "DESERIALIZE-~a" upcase-name) their-package))
       (deserialize-or-passthrough-enum (intern (format nil "DESERIALIZE-OR-PASSTHROUGH-~a" upcase-name) their-package)))
    `(progn
       (defparameter ,alist-name
         (list ,@(loop :for variant :in variants
                       :collect `(cons ,(first variant) ,(second variant)))))
       (defun ,serialize-enum (sym) (cdr (assoc sym ,alist-name)))
       (defun ,serialize-or-passthrough-enum (sym) (or (cdr (assoc sym ,alist-name)) sym))
       (defun ,deserialize-enum (value) (car (rassoc value ,alist-name)))
       (defun ,deserialize-or-passthrough-enum (value) (or (car (rassoc value ,alist-name)) value)))))

(defenumfield message-code
  (:get             001)
  (:post            002)
  (:put             003)
  (:delete          004))

(defun make-message-code (class detail) (+ (* 100 class) detail))
(defun decompose-message-code (code) (floor code 100))

(defenumfield message-type
  (:confirmable     0)
  (:non-confirmable 1)
  (:acknowledgement 2)
  (:reset           3))

(defenumfield option-type
 (:if-match         1)
 (:uri-host         3)
 (:etag             4)
 (:if-none-match    5)
 (:uri-port         7)
 (:location-path    8)
 (:uri-path         11)
 (:content-format   12)
 (:max-age          14)
 (:uri-query        15)
 (:accept           17)
 (:location-query   20)
 (:proxy-uri        35)
 (:proxy-scheme     39)
 (:size1            60))

(defparameter *knownn-option-formats*
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
   (:size1 . :uint)))

(defun be32->int (bytes)
  (loop :for b :in bytes
        :for shift :from (* (1- (length bytes)) 8) :downto 0 :by 8
        :sum (ash b shift)))

(defun parse-coap-packet (pkt)
  (let*
      ((stream (byte-stream:make-bytes-input-stream pkt))
       (hdr (loop :for i :from 0 :below 4 :collect (read-byte stream)))
       (version (ldb (byte 2 6) (first hdr)))
       (typ (ldb (byte 2 4) (first hdr)))
       (token-length (ldb (byte 4 0) (first hdr)))
       (code-class (ldb (byte 3 5) (second hdr)))
       (code-detail (ldb (byte 5 0) (second hdr)))
       (message-id (logior (ash (third hdr) 8) (fourth hdr)))
       (token (be32->int (loop :for i :from 0 :below token-length :collect (read-byte stream))))
       (option-number 0)
       (option nil)
       (options (list)))
    (loop
      :while (and (< index (length pkt)) (/= (elt pkt index) #xff))
      :do
      (multiple-value-setq (index option-number option) (deserialize-option pkt index option-number))
      (push option options))
    (incf index) ;cinsume #xff
    (make-packet
      :version version
      :type typ
      :token-length token-length
      :token token
      :code (cons code-class code-detail)
      :id message-id
      :options (nreverse options)
      :payload (if (< index (length pkt)) (subseq pkt index) nil))))

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
    (14 (+ (logior (ash (read-byte stream) 8) (read-byte stream) 269)))
    (15 (error "invalid value for varlen field header"))
    (otherwise value-or-varlen)))

(defun deserialize-option-value (option-type arr)
  (case (cdr (assoc option-type *knownn-option-formats*))
    (:uint (collect-bytes-nbo arr))
    (:string (map 'string #'code-char arr))
    (:opaque arr)
    (otherwise arr)))

(defun deserialize-option (stream current-option-number)
  (let* ((delta-length-header (read-byte stream))
         (option-delta (read-varlen-field stream (ldb (byte 4 4) delta-length-header)))
         (option-length (read-varlen-field stream (ldb (byte 4 0) delta-length-header)))
         (new-option-number (+ current-option-number option-delta))
         (option-type (or (deserialize-option-type current-option-number) current-option-number)))
    (values
      (make-option
        :type option-type
        :value (deserialize-option-value option-type (byte-stream:read-bytes stream option-length))
        :serialized-length option-length)
      new-option-number)))
                 

(defun serialize-option-payload (option)
  (etypecase (option-value option)
    (integer
      (let* ((bytes (or (option-serialized-length option) 4))
             (arr (make-array bytes :element-type '(unsigned-byte 8))))
        (loop :for i :from 0 :below bytes
              :for shift :from (* (1- bytes) 8) :downto 0 :by 8
              :do (setf (elt arr i) (logand #xff (ash (option-value option) (- shift)))))
        arr))
    (string (map '(vector (unsigned-byte 8)) #'char-code (option-value option)))
    ((array (unsigned-byte 8)) (option-value option))))

(defun serialize-option (pkt current-opt-number option)
  (vector-push-extend 0 pkt) ; encoded header
  (let*
      ((del-len-idx (1- (length pkt)))
       (serialized-option-type (serialize-or-passthrough-option-type (option-type option)))
       (delta (- serialized-option-type current-opt-number))
       (encoded-delta
         (cond
           ((> delta 269)
            (let ((encoded (- delta 269)))
              (vector-push-extend (ash encoded -8) pkt)
              (vector-push-extend (logand encoded #xff) pkt)
              14))
           ((>= delta 13)
            (let ((encoded (- delta 13)))
              (vector-push-extend encoded pkt)
              13))
           (t delta)))
       (serialized-payload (serialize-option-payload option))
       (unencoded-length (length serialized-payload))
       (encoded-length
         (cond
           ((> unencoded-length 269)
            (let ((encoded (- unencoded-length 269)))
              (vector-push-extend (ash encoded -8) pkt)
              (vector-push-extend (logand encoded #xff) pkt)
              14))
           ((>= unencoded-length 13)
            (let ((encoded (- unencoded-length 13)))
              (vector-push-extend encoded pkt)
              13))
           (t unencoded-length))))
    (setf (elt pkt del-len-idx) (logior (ash encoded-delta 4) encoded-length))
    (loop :for byte :across serialized-payload :do (vector-push-extend byte pkt))
    (+ current-opt-number serialized-option-type)))


(defparameter pp (make-array 10 :adjustable t :fill-pointer 0))
pp
(serialize-option pp 0 (make-option :type :uri-path :value "The value is longer than fourteen characters"))
(deserialize-option pp 0 0)
