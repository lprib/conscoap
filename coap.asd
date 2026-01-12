(require :asdf)
(asdf:defsystem "coap"
  :description "A CoAP implementation for common lisp"
  :version "0.1.0"
  :author "Liam Pribis <j a c k p r i b i s @ g m a i l . c o m>"
  :license "LGPL-3.0-or-later"
  :source-control (:git "https://github.com/lprib/conscoap")
  :serial t
  :depends-on (:usocket)
  :components
      ((:file "package")
       (:file "byte-stream")
       (:file "pdu")
       (:file "application-layer")
       (:file "server")
       (:file "client")))
       
