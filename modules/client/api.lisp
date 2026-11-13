(defpackage #:open-orders.client.api
  (:use #:cl #:open-orders.common.fn #:open-orders.client.fetch)
  (:export
   #:api-create
   #:api-delete
   #:api-query
   #:api-query-components
   #:api-get
   #:api-set
   #:api-get-components
   #:api-set-components
   #:api-unset
   #:api-login))
(in-package #:open-orders.client.api)

(defun parse (http-result-body)
  (when (stringp http-result-body)
    (let ((*read-eval* nil))
      (handler-case 
          (read-from-string http-result-body)
        (parse-error (e)
          (print e)
          nil)))))

(fn (api-get (values)) ((auth-token string)
                        (id integer)
                        (component keyword)
                        &optional
                        ((callback (or null (function (t integer) t)))))
  (fetch "get" (list (cons "auth-token" auth-token)
                      (cons "id" (write-to-string id))
                      (cons "component" (write-to-string component)))
         (lambda (result http-code)
           (when callback
             (funcall callback (parse result) http-code))))
  (values))

(fn (api-login (values)) ((username string)
                          (password string)
                          &optional
                          ((callback (or null (function (t integer) t)))))
  (fetch "login" (list (cons "username" username)
                        (cons "password" password))
         (lambda (result http-code)
           (when callback (funcall callback (parse result) http-code))))
  (values))

(fn (api-set (values)) ((auth-token string)
                        (id integer)
                        (component keyword)
                        (value t)
                        &optional
                        ((callback (or null (function (t integer) t)))))
  (fetch "set" (list (cons "auth-token" auth-token)
                      (cons "id" (write-to-string id))
                      (cons "component" (write-to-string component))
                      (cons "value" (write-to-string value)))
         (lambda (result http-code)
           (when callback (funcall callback (parse result) http-code))))
  (values))

(fn (api-unset (values)) ((auth-token string)
                          (id integer)
                          (component keyword)
                          &optional
                          ((callback (or null (function (t integer) t)))))
  (fetch "unset" (list (cons "auth-token" auth-token)
                        (cons "id" (write-to-string id))
                        (cons "component" (write-to-string component)))
         (lambda (result http-code)
           (when callback (funcall callback (parse result) http-code))))
  (values))

(fn (api-query (values)) ((auth-token string)
                          (components (vector keyword))
                          &optional
                          ((callback (or null (function (t integer) t)))))
  (fetch "query" (list (cons "auth-token" auth-token)
                        (cons "components" (write-to-string components)))
         (lambda (result http-code)
           (when callback (funcall callback (parse result) http-code))))
  (values))

(fn (api-query-ext (values)) ((auth-token string)
                              (components (vector keyword))
                              (sort-by keyword)
                              (offset integer)
                              (limit integer)
                              &optional
                              ((callback (or null (function (t integer) t)))))
  (fetch "query-ext"
         (list (cons "auth-token" auth-token)
               (cons "components" (write-to-string components))
               (cons "sort-by" (write-to-string sort-by))
               (cons "offset" (write-to-string offset))
               (cons "limit" (write-to-string limit)))
         (lambda (result http-code)
           (when callback (funcall callback (parse result) http-code))))
  (values))

(fn (api-query-components (values)) ((auth-token string)
                                     (components (vector keyword))
                                     &optional
                                     ((callback (or null (function (t integer) t)))))
  (fetch "query-components"
         (list (cons "auth-token" auth-token)
               (cons "components" (write-to-string components)))
         (lambda (result http-code)
           (when callback (funcall callback (parse result) http-code))))
  (values))

(fn (api-get-components (values)) ((auth-token string)
                                   (id integer)
                                   &optional
                                   ((callback (or null (function (t integer) t)))))
  (fetch "get-components"
         (list (cons "auth-token" auth-token)
               (cons "id" (write-to-string id)))
         (lambda (result http-code)
           (when callback (funcall callback (parse result) http-code))))
  (values))

(fn (api-set-components (values)) ((auth-token string)
                                   (components-plist list)
                                   &optional
                                   ((callback (or null (function (t integer) t)))))
  (fetch "set-components"
         (list (cons "auth-token" auth-token)
               (cons "components-plist"
                     (write-to-string components-plist)))
         (lambda (result http-code)
           (when callback (funcall callback (parse result) http-code))))
  (values))

(fn (api-create (values)) ((auth-token string)
                           &optional
                           ((callback (or null (function (t integer) t)))))
  (fetch "create" (list (cons "auth-token" auth-token))
         (lambda (result http-code)
           (when callback (funcall callback (parse result) http-code))))
  (values))

(fn (api-delete (values)) ((auth-token string)
                           (id integer)
                           &optional
                           ((callback (or null (function (t integer) t)))))
  (fetch "delete" (list (cons "auth-token" auth-token)
                         (cons "id" (write-to-string id)))
         (lambda (result http-code)
           (when callback (funcall callback (parse result) http-code))))
  (values))

