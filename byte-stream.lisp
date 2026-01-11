(defpackage :byte-stream
  (:use :cl)
  (:export
    :read-bytes
    :peek-byte
    :make-bytes-input-stream
    :make-bytes-output-stream
    :stream-buffer))
(in-package :byte-stream)

; Implements gray binary input stream for an underlying (unsigned-char 8) array
(defclass bytes-input-stream (sb-gray:fundamental-binary-input-stream)
  ((data :initarg :data :reader stream-data)
   (position :initform 0 :accessor stream-position)))

(defmethod sb-gray:stream-read-byte ((stream bytes-input-stream))
  (with-accessors ((data stream-data) (position stream-position)) stream
    (if (< position (length data))
        (prog1 (aref data position)
          (incf position))
        :eof)))

(defmethod read-bytes ((stream bytes-input-stream) count &optional (eof-error-p t) (eof-value nil))
  "Read count bytes from the stream, returning a displaced array"
  (with-accessors ((data stream-data) (position stream-position)) stream
    (if (<= (+ position count) (length data))
        (prog1 (make-array count
                           :element-type (stream-element-type stream)
                           :displaced-to data
                           :displaced-index-offset position)
          (incf position count))
        (if eof-error-p
            (error 'end-of-file :stream stream)
            eof-value))))

(defmethod stream-element-type ((stream bytes-input-stream)) '(unsigned-byte 8))

(defmethod peek-byte ((stream bytes-input-stream) &optional (eof-error-p t) (eof-value nil))
  "Like peek-char but for binary stream"
  (with-accessors ((data stream-data) (position stream-position)) stream
    (if (< position (length data))
        (aref data position)
        (if eof-error-p
            (error 'end-of-file :stream stream)
            eof-value))))

(defun make-bytes-input-stream (bytes) (make-instance 'bytes-input-stream :data bytes))

(defclass bytes-output-stream (sb-gray:fundamental-binary-output-stream)
  ((buffer
     :initform (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)
     :accessor stream-buffer)))
(defmethod sb-gray:stream-write-byte ((stream bytes-output-stream) integer)
  (vector-push-extend integer (stream-buffer stream)))

(defun make-bytes-output-stream () (make-instance 'bytes-output-stream))
