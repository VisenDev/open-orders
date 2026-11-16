(defpackage #:open-orders.server.api
  (:use #:cl)
  (:local-nicknames (#:db #:open-orders.server.db))
  (:export
   #:main
   #:stop
   #:start))
(in-package #:open-orders.server.api)

(define-condition api-error (error)
  ((code :initarg :code :reader api-error-code)
   (message :initarg :message :reader api-error-message)))

(defun fail-api (code control &rest arguments)
  (error 'api-error
         :code code
         :message (apply #'format nil control arguments)))

(defun decode-api-parameter (name text type)
  ;; NIL is a valid decoded value; a missing parameter is not.
  (unless (stringp text)
    (fail-api 400 "Missing parameter: ~A" name))
  (let ((value
          (handler-case
              (with-standard-io-syntax
                (let ((*read-eval* nil))
                  (with-input-from-string (stream text)
                    (let* ((eof (gensym "EOF"))
                           (value (read stream nil eof)))
                      (when (or (eq value eof)
                                (not (eq (read stream nil eof) eof)))
                        (fail-api 400
                                  "Expected one Lisp value for ~A" name))
                      value))))
            (reader-error ()
              (fail-api 400 "Malformed parameter: ~A" name))
            (end-of-file ()
              (fail-api 400 "Incomplete parameter: ~A" name)))))
    (unless (typep value type)
      (fail-api 400 "Invalid parameter ~A; expected ~S" name type))
    value))

(defun component-vector-p (value)
  (and (vectorp value)
       (not (stringp value))
       (every #'keywordp value)))

(deftype component-vector ()
  '(satisfies component-vector-p))

(defun component-plist-p (value)
  ;; LIST-LENGTH also rejects circular lists.
  (and (listp value)
       (handler-case
           (let ((length (list-length value)))
             (and length
                  (evenp length)
                  (loop for (key val) on value by #'cddr
                        always (keywordp key))))
         (type-error () nil))))

(deftype component-plist ()
  '(satisfies component-plist-p))

;; Dynamically bound for each request. Bodies can supply audit context.
(defvar *api-user* nil)
(defvar *api-entity-id* nil)

(defun api-authenticated-user (token)
  ;; This deliberately avoids AUTH-TOKEN->USERNAME's incorrect STRING
  ;; return declaration: an unknown token legitimately produces NIL.
  (or (sqlite:execute-single
       db:*db*
       "select username from user where auth_token = ?;"
       token)
      (fail-api 403 "Not authorized")))

(defmacro defapi ((name uri &key (authenticated t))
                  parameters
                  &body body)
  "PARAMETERS contains (name type) pairs.
Authenticated handlers implicitly accept AUTH-TOKEN.
BODY returns a Lisp value, which is serialized once."
  (let* ((token (gensym "TOKEN"))
         (wire-parameters
           (if authenticated
               (cons (list token 'string) parameters)
               parameters))
         (raw-bindings
           (loop for (parameter type) in wire-parameters
                 collect (list parameter (gensym "RAW"))))
         (category (string-left-trim "/" uri))
         (result (gensym "RESULT"))
         (condition (gensym "CONDITION")))
    `(hunchentoot:define-easy-handler
         (,name :uri ,uri :default-request-type :post)
         ,(loop for (parameter raw) in raw-bindings
                collect
                `(,raw :real-name
                       ,(if (eq parameter token)
                            "auth-token"
                            (string-downcase (symbol-name parameter)))))
       (setf (hunchentoot:content-type*)
             "text/plain; charset=utf-8")
       (let ((*api-user* nil)
             (*api-entity-id* nil))
         (handler-case
             (let* (
                    ,@(when authenticated
                        `((,token
                            (decode-api-parameter
                             "auth-token"
                             ,(second (assoc token raw-bindings))
                             'string))
                          (*api-user*
                            (api-authenticated-user ,token))))
                    ,@(loop for (parameter type) in parameters
                            collect
                            `(,parameter
                              (decode-api-parameter
                               ,(string-downcase (symbol-name parameter))
                               ,(second (assoc parameter raw-bindings))
                               ',type))))
               (let ((,result (progn ,@body)))
                 (db:log :username *api-user*
                         :category ,category
                         :entity_id *api-entity-id*
                         :details "success")
                 (with-standard-io-syntax
                   (write-to-string ,result))))
           (api-error (,condition)
             (setf (hunchentoot:return-code*)
                   (api-error-code ,condition))
             (db:log :username *api-user*
                     :category ,category
                     :entity_id *api-entity-id*
                     :details
                     (format nil "failure: ~A"
                             (api-error-message ,condition)))
             (with-standard-io-syntax
               (write-to-string (api-error-message ,condition))))
           (db:invalid-entity ()
             (setf (hunchentoot:return-code*) 400)
             (db:log :username *api-user*
                     :category ,category
                     :entity_id *api-entity-id*
                     :details "failure: invalid entity")
             (write-to-string "Invalid entity")))))))

(defapi (api-login "/login" :authenticated nil)
    ((username string)
     (password string))
    (let ((token (db:user-get-auth-token username password)))
      (unless token
        (fail-api 401 "Incorrect username or password"))
      (setf *api-user* username)
      token))

(defapi (api-get "/get")
    ((id (integer 0 *))
     (component keyword))
  (setf *api-entity-id* id)
  (db:get (db:make-entity :id id) component))

(defapi (api-set "/set")
    ((id (integer 0 *))
     (component keyword)
     (value t))
  (when (eq component :id)
    (fail-api 400 "The ID component cannot be changed"))
  (setf *api-entity-id* id)
  (db:set (db:make-entity :id id) component value)
  nil)

(defapi (api-unset "/unset")
    ((id (integer 0 *))
     (component keyword))
  (when (eq component :id)
    (fail-api 400 "The ID component cannot be removed"))
  (setf *api-entity-id* id)
  (db:unset (db:make-entity :id id) component)
  nil)

(defapi (api-query "/query")
    ((components component-vector))
  (db:query components))

(defapi (api-query-ext "/query-ext")
    ((components component-vector)
     (sort-by keyword)
     (offset (integer 0 *))
     (limit (integer 0 *)))
  (db:query-ext components
                :sort-by sort-by
                :offset offset
                :limit limit))

(defapi (api-query-components "/query-components")
    ((components component-vector))
  (db:query-components components))

(defapi (api-get-components "/get-components")
    ((id (integer 0 *)))
  (setf *api-entity-id* id)
  (db:get-components (db:make-entity :id id)))

(defapi (api-set-components "/set-components")
    ((components-plist component-plist))
  (let ((id (getf components-plist :id)))
    (unless (typep id '(integer 0 *))
      (fail-api 400 "components-plist requires an integer :ID"))
    (setf *api-entity-id* id)
    (db:set-components components-plist)
    nil))

(defapi (api-create "/create") ()
  (let ((id (db:entity-id (db:create))))
    (setf *api-entity-id* id)
    id))

(defapi (api-delete "/delete")
    ((id (integer 0 *)))
  (setf *api-entity-id* id)
  (db:delete (db:make-entity :id id))
  nil)

(let (acceptor)
  (defun start ()
    (db:connect)
    (when acceptor
      (hunchentoot:stop acceptor))
    (setf acceptor (make-instance 'hunchentoot:easy-acceptor :port 8888))

    (hunchentoot:start acceptor))

  (defun stop ()
    (db:disconnect)
    (when acceptor
      (hunchentoot:stop acceptor))
    (setf acceptor nil))

  (defun main ()
    (start)
    (unwind-protect (loop (sleep 1))
      (stop))))
