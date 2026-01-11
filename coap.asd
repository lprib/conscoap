(require :asdf)
(asdf:defsystem "coap"
  :serial t
  :depends-on (:usocket)
  :components
      ((:file "byte-stream")
       (:file "coap")))
       
