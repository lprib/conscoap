(in-package :coap)

(defmacro defenumfield (name &rest variants)
  "Generate an associative enum with symbols (names of enum variante) and values (values of enum variants)
  Syntax:
    (defenumfield name
      (:variant value)
      (:variant2 value))
  Generates the following functions:
    (defun serialize-<name> (symbol) value)
    (defun serialize-or-passthrough-<name> (symbol-or-value) value) ; same as above, but it symbol is not found, pass through the value
    (defun dserialize-<name> (value) sybol)
    (defun dserialize-or-passthrough-<name> (symbol-or-value) symbol) ; same as above, but it symbol is not found, pass through the value
  "
  (let*
      ((alist-name (gensym))
       (upcase-name (string-upcase name))
       (their-package (symbol-package name))
       (serialize-enum (intern (format nil "SERIALIZE-~a" upcase-name) their-package))
       (serialize-or-passthrough-enum (intern (format nil "SERIALIZE-OR-PASSTHROUGH-~a" upcase-name) their-package))
       (deserialize-enum (intern (format nil "DESERIALIZE-~a" upcase-name) their-package))
       (deserialize-or-passthrough-enum (intern (format nil "DESERIALIZE-OR-PASSTHROUGH-~a" upcase-name) their-package)))
    `(progn
       (defparameter ,alist-name
         (list ,@(loop :for variant :in variants
                       :collect `(cons ,(first variant) ,(second variant)))))
       (defun ,serialize-enum (sym) (cdr (assoc sym ,alist-name)))
       (defun ,serialize-or-passthrough-enum (sym) (or (cdr (assoc sym ,alist-name)) sym))
       (defun ,deserialize-enum (value) (car (rassoc value ,alist-name)))
       (defun ,deserialize-or-passthrough-enum (value) (or (car (rassoc value ,alist-name)) value)))))

