# conscoap: CoAP library for common lisp
This library implements the Constrained Application Protocol
([RFC7252](https://datatracker.ietf.org/doc/html/rfc7252)) in common lisp.


## Example
### Server
This implements a resource /thing/test that responds to get and post requests.

```lisp
(defparameter *server* (make-instance 'coap:server :ip "0.0.0.0" :port 5683))

(coap:server-add-resource *server*
  (coap:make-resource "/thing/test"
    :get-handler
      (lambda (request) (declare (ignore request)) (coap:make-response :content "Hello"))
    :post-handler
      (lambda (request) (declare (ignore request)) (coap:make-response :changed "posted up"))))

(coap:server-listen-once *server*)
```

### Client

```lisp
(coap:get-request "coap://localhost/example_data")
```


## Source files
- `pdu.lisp`: packet serialization and deserialization to lisp data
  structures
- `application-layer.lisp` shared client and server code for higher-level
  protocol implementation
- `server.lisp`: CoAP server
- `client.lisp`: CoAP client
- `byte-stream.lisp` gray streams implementation for streams backed by a byte
  buffer
- `enum-field.lisp`: Macro for making enumerations for defining the protocols,
  eg enumerating all of the response codes.

## Features
- [x] CoAP packet parsing and serializing
- [x] CoAP server
- [x] CoAP client
- [ ] Token tracking and retransmits
- [ ] Block transfer extension
- [ ] Observable extension
