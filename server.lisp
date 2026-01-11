(in-package :coap)

;(defparameter *server* (usocket:socket-connect nil nil :protocol :datagram :element-type '(unsigned-byte 8) :local-host "0.0.0.0" :local-port 5683))

(defun hex (arr)
 (loop :for b :across arr :do (format t "~x " b))
 (format t "~%"))

;(multiple-value-bind (buf len host port) (usocket:socket-receive *server* nil 4096)
; (declare (ignore host port))
; (let*
;     ((pkt (make-array len :element-type '(unsigned-byte 8) :displaced-to buf))
;      (des (deserialize-coap-packet pkt))
;      (strm (byte-stream:make-bytes-output-stream))
;      (ssss (serialize-coap-packet strm des))
;      (ser (byte-stream:stream-buffer strm)))
;   (when (equalp pkt ser) (format t "OK"))))

