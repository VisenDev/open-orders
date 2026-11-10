(defpackage #:open-orders.client.fetch
  (:use #:cl)
  (:export
   #:start
   #:stop
   #:fetch))
(in-package #:open-orders.client.fetch)

(defvar *server-url* "http://localhost:8888")

#-jscl
(progn
  (defvar *channel* nil)

  (defun start ()
    (setf lparallel:*kernel* (lparallel:make-kernel 2))
    (setf *channel* (lparallel:make-channel)))

  (defun stop ()
    (setf *channel* nil)
    (lparallel:end-kernel :wait t))

  (defun fetch (subdomain parameters-alist result-handler)
    (lparallel:submit-task
     *channel*
     (lambda ()
       (multiple-value-bind (result http-code)
           (drakma:http-request
            (format nil "~a/~a" *server-url* subdomain)
            :parameters parameters-alist
            :form-data t
            :method :post)
         (funcall result-handler result http-code))))))

#+jscl
(progn
  (defun start ()
    (values))

  (defun stop ()
    (values))

  (defun fetch (subdomain parameters-alist result-handler)
    (let ((url
            (#j:make-new
             #j:URL
             (format nil "~a/~a" *server-url* subdomain))))
      (dolist (parameter parameters-alist)
        (#j:append
         (#j:searchParams url)
         (car parameter)
         (cdr parameter)))

      (#j:then
       (#j:then
        (#j:fetch (#j:toString url))
        (lambda (response)
          (#j:text response)))
       (lambda (body)
         (funcall result-handler body))))))
