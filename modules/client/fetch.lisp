(uiop:define-package #:open-orders.client.fetch
  (:use #:cl #:open-orders.common.fn)
  (:export
   #:start
   #:stop
   #:fetch
   #:execute-result-handlers
   #:api-login
   #:api-login-cached
   #:with-api-login
   #:api-get
   #:api-get-cached
   #:with-api-get
   #:api-set
   #:api-set-cached
   #:with-api-set
   #:api-unset
   #:api-unset-cached
   #:with-api-unset
   #:api-query
   #:api-query-cached
   #:with-api-query
   #:api-query-ext
   #:api-query-ext-cached
   #:with-api-query-ext
   #:api-query-components
   #:api-query-components-cached
   #:with-api-query-components
   #:api-get-components
   #:api-get-components-cached
   #:with-api-get-components
   #:api-set-components
   #:api-set-components-cached
   #:with-api-set-components
   #:api-create
   #:api-create-cached
   #:with-api-create
   #:api-delete
   #:api-delete-cached
   #:with-api-delete
   #:clear-cache))
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun first-or-self (object)
    (if (listp object) (first object) object)))

(defmacro defapi (name endpoint (&rest parameters))
  (let ((parameter-names (mapcar #'first-or-self parameters)))
    `(progn

       (fn (,name (values))
           ((handler (function (t integer) t)) &key ,@(mapcar #'list parameters))
         
         (fetch ,endpoint handler ,@(mapcan (lambda (name)
                                              (list (intern (symbol-name name)
                                                            :keyword)
                                                    name))
                                            parameter-names))
         (values))
       
       (defmacro ,(symbolicate 'with- name)
           ((&key ,@parameter-names) (result code) &body body)
         `(,',name (lambda (,result ,code)
                     (declare (ignorable ,result ,code))
                     ,@body)
                   ,,@(mapcan (lambda (name)
                                              (list (intern (symbol-name name)
                                                            :keyword)
                                                    name))
                                            parameter-names))))))

(defapi api-login "login" ((username string) (password string)))
(defapi api-get "get" ((auth-token string) (id integer) (component keyword)))
(defapi api-set "set" ((auth-token string) (id integer) (component keyword) (value t)))
(defapi api-unset "unset" ((auth-token string) (id integer) (component keyword)))
(defapi api-query "query" ((auth-token string) (components (vector keyword))))
(defapi api-query-ext "query-ext" ((auth-token string) (components (vector keyword)) (sort-by keyword) (offset integer) (limit integer)))
(defapi api-query-components "query-components"((auth-token string) (components (vector keyword))))
(defapi api-get-components "get-components" ((auth-token string) (id integer)))
(defapi api-set-components "set-components" ((auth-token string) (components-plist list)))
(defapi api-create "create" ((auth-token string)))
(defapi api-delete "delete" ((auth-token string) (id integer)))
