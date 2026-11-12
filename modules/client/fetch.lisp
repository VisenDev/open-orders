(defpackage #:open-orders.client.fetch
  (:use #:cl)
  (:export
   #:start
   #:stop
   #:fetch
   #:execute-result-handlers))
(in-package #:open-orders.client.fetch)

(defvar *server-url* "http://localhost:8888")

#-jscl
(progn
  (defvar *channel* nil)
  (defvar *result-handlers* nil)
  (defvar *result-lock*
    (bt:make-lock "fetch-result-lock"))

  (defun start ()
    (setf lparallel:*kernel* (lparallel:make-kernel 2)
          *channel* (lparallel:make-channel)
          *result-handlers* nil))

  (defun stop ()
    (lparallel:end-kernel :wait t)
    (setf *channel* nil)
    (bt:with-lock-held (*result-lock*)
      (setf *result-handlers* nil)))

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
         (bt:with-lock-held (*result-lock*)
           (push (lambda ()
                   (funcall result-handler result http-code))
                 *result-handlers*))))))

  (defun execute-result-handlers ()
    "Execute completed HTTP result handlers on the calling thread."
    (let ((handlers
            (bt:with-lock-held (*result-lock*)
              (prog1 (nreverse *result-handlers*)
                (setf *result-handlers* nil)))))
      (dolist (handler handlers)
        (funcall handler)))))

#+jscl
(progn
  (defvar *result-handlers* nil)

  (defun start ()
    (setf *result-handlers* nil)
    (values))

  (defun stop ()
    (setf *result-handlers* nil)
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
       (#j:fetch (#j:toString url)
                 (lisp-to-js
                  (list :method "POST")))
       (lambda (response)
         (let ((code (#j:status response)))
           (#j:then
            (#j:text response)
            (lambda (body)
              (push (lambda ()
                      (funcall result-handler body code))
                    *result-handlers*))))))))

  (defun execute-result-handlers ()
    "Execute completed HTTP result handlers."
    (let ((handlers (nreverse *result-handlers*)))
      (setf *result-handlers* nil)
      (dolist (handler handlers)
        (funcall handler)))))
