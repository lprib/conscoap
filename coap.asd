(require :asdf)
(asdf:defsystem "coap"
  :serial t
  :depends-on (:usocket)
  :components
      ((:file "package")
       (:file "byte-stream")
       (:file "binary-format")))
       
