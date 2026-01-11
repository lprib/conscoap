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

    :make-packet
    :packet-version
    :packet-type
    :packet-token-length
    :packet-token
    :packet-id
    :packet-options
    :packet-payload

    :make-message-code
    :decompose-message-code
    :deserialize-coap-packet
    :serialize-coap-packet))

        

