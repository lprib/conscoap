# conscoap: CoAP library for common lisp
This library implements the Constrained Application Protocol
([RFC7252](https://datatracker.ietf.org/doc/html/rfc7252)) in common lisp.

## Source files
- `binary-format.lisp`: packet serialization and deserialization to lisp data
  structures
- `server.lisp`: CoAP server
- `byte-stream.lisp` gray streams implementation for streams backed by a byte
  buffer

## Features and TODO
- [x] CoAP packet parsing and serializing
- [x] CoAP server
- [ ] Token tracking
- [ ] CoAP client
- [ ] Block transfer extension
- [ ] Observable extension
