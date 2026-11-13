(defpackage #:open-orders.client.api
  (:use #:cl #:open-orders.common.fn #:open-orders.client.fetch)
  (:shadow #:set #:get #:delete #:log))
(in-package #:open-orders.client.api)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun first-or-identity (object) (if (listp object)
                                        (first object)
                                        object)))

(defmacro defapi (url name parameters)
  `(fn ,name ,(append parameters `((callback (function (t t) t))))
     (fetch ,url (list ,@(mapcar (lambda (arg)
                                   `(cons ,(string-downcase (format nil ":~a" arg))
                                          (format nil "~S" ,arg)))
                                 (mapcar #'first-or-identity parameters)))
            callback)))

(defapi "/get" get ((auth-token string) (id integer) (component keyword)))
(defapi "/set" set ((auth-token string) (id integer) (component keyword) (value t)))
