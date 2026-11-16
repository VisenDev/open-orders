(defpackage #:open-orders.client.fetch
  (:use #:cl #:open-orders.common.fn)
  (:export
   #:start
   #:stop
   #:fetch
   #:execute-result-handlers
   #:with-fetch
   #:encode-parameters
   #:fetch*))
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

  (defun %fetch (subdomain parameters-alist result-handler)
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

  (defun %fetch (subdomain parameters-alist result-handler)
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


(defun encode-parameters (parameters)
  (unless (evenp (length parameters))
    (error "Expected keyword/value pairs."))
  (sort
   (loop :for (name value) :on parameters :by #'cddr
         :do (unless (keywordp name)
              (error "Expected a keyword, got ~S." name))
         :collect
         (cons (string-downcase (symbol-name name))
               (with-standard-io-syntax
                     (write-to-string value))))
   #'string< :key #'car))

(fn (parse t) ((http-result (or string null)))
  (when (stringp http-result)
    (let ((*read-eval* nil))
      (handler-case 
          (read-from-string http-result)
        (parse-error (e)
          (print e)
          nil)
        (end-of-file ()
          nil)))))

(defun fetch (endpoint callback &rest parameters)
  (%fetch endpoint
          (encode-parameters parameters)
          (lambda (raw-result code)
            (funcall callback (parse raw-result) code))))

(defmacro defapi (name endpoint (&rest parameters))
  `(defmacro ,name ((&rest args &key ,@parameters) (result code) &body body)
     (declare (ignore ,@parameters))
     `(funcall #'fetch ,,endpoint (lambda (,result ,code)
                                    (declare (ignorable ,result ,code))
                                    ,@body)
               ,args)))

(defapi api-get "/get" (auth-token id component))

;; (defun fetch* (endpoint &rest parameters)
;;   "fetch except results are discarded"
;;   (apply #'fetch endpoint
;;            (lambda (_ __) (declare (ignore _ __)))
;;            parameters))

;; (defmacro with-fetch ((endpoint &rest parameters)
;;                       (http-result code)
;;                       &body body)
;;   `(fetch ,endpoint
;;           (lambda (,http-result ,code)
;;             (declare (ignorable ,http-result ,code))
;;             ,@body)
;;           ,@parameters))
