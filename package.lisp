(defpackage :byte-stream
  (:use :cl)
  (:export
    :read-bytes
    :peek-byte
    :make-bytes-input-stream
    :make-bytes-output-stream
    :stream-buffer))

(defpackage :coap
  (:use :cl)
  (:export
    :make-option
    :option-type
    :option-value
    :option-serialized-length

    :make-pdu
    :pdu-version
    :pdu-type
    :pdu-token-length
    :pdu-token
    :pdu-id
    :pdu-options
    :pdu-payload

    :response
    :make-response
    :response-code
    :response-payload

    :server
    :server-listen-once
    :server-register-handler))

