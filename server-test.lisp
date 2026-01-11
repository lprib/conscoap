(defpackage :coap-server-test
  (:use
    :cl
    :coap))

(in-package :coap-server-test)

(defparameter *server* (make-instance 'server :ip #(0 0 0 0) :port 8888))

(defun my-handler (request)
  (declare (ignore request))
  (make-response :content "hello there"))

(server-register-handler *server* "a/b" #'my-handler)
(server-listen-once *server*)

