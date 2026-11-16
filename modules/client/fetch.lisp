(uiop:define-package #:open-orders.client.fetch
  (:use #:cl #:open-orders.common.fn)
  (:export
   #:start
   #:stop
   #:fetch
   #:execute-result-handlers
   ;; #:with-fetch
   ;; #:encode-parameters
   ;; #:fetch*
   #:api-login
   #:api-get
   #:api-set
   #:api-unset
   #:api-query
   #:api-query-ext
   #:api-query-components
   #:api-get-components
   #:api-set-components
   #:api-create
   #:api-delete))
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
               ,@args)))

(defapi api-login "login" (username password))
(defapi api-get "get" (auth-token id component))
(defapi api-set "set" (auth-token id component value))
(defapi api-unset "unset" (auth-token id component))
(defapi api-query "query" (auth-token components))
(defapi api-query-ext "query-ext" (auth-token components sort-by offset limit))
(defapi api-query-components "query-components"(auth-token components))
(defapi api-get-components "get-components" (auth-token id))
(defapi api-set-components "set-components" (auth-token components-plist))
(defapi api-create "create" (auth-token))
(defapi api-delete "delete" (auth-token id))
