(defpackage #:open-orders.utils
  (:use #:cl)
  (:export #:*let
           #:fn))
(in-package #:open-orders.utils)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun ignored-binding-p (binding)
    (and (listp binding)
         (char= #\_ (char (symbol-name (first binding)) 0)))))

(defmacro *let (bindings &body body)
  "let* except it allows underscore prefixed vars to be ignored automatically"
  (let (ignored)
    (mapcar (lambda (binding)
              (when (ignored-binding-p binding)
                  (push (first binding) ignored)))
            bindings)
    `(let* ,bindings
       (declare (ignorable ,@ignored))
       ,@body)))

(defmacro fn (args &body body)
  "Shorter lambda that automatically makes args ignorable"
  `(lambda ,args
     (declare (ignorable ,@args))
     ,@body))
