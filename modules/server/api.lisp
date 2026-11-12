(defpackage #:open-orders.server.api
  (:use #:cl)
  (:local-nicknames (#:db #:open-orders.server.db))
  (:export
   #:main
   #:stop
   #:start))
(in-package #:open-orders.server.api)

(defun parse-id-or-nil (id)
  (handler-case (parse-integer id)
    (parse-error () nil)))

(hunchentoot:define-easy-handler (api-login :uri "/login") (username password)
  (setf (hunchentoot:content-type*) "text/plain")
  (cond ((db:user-password-correct-p username password)
         (db:log :username username
                 :category "login"
                 :details "success")
         
         (format nil "~S" (db:user-get-auth-token username password)))

        (t
         (db:log :username username
                 :category "login"
                 :details "failure")
         (setf (hunchentoot:return-code*)
               hunchentoot:+HTTP-AUTHORIZATION-REQUIRED+)
         "")))

(hunchentoot:define-easy-handler (api-get :uri "/get") (auth-token id component)
  (setf (hunchentoot:content-type*) "text/plain")
  (let ((parsed (parse-id-or-nil id)))
    (cond ((db:user-auth-token-valid-p auth-token)
           (cond ((null parsed)
                  (setf (hunchentoot:return-code*) hunchentoot:+HTTP-BAD-REQUEST+)
                  (db:log :username (db:auth-token->username auth-token)
                          :category "get"
                          :details "failure: invalid id")
                  "Invalid id parameter!")

                 ((or (null component) (string= component ""))
                  (setf (hunchentoot:return-code*) hunchentoot:+HTTP-BAD-REQUEST+)
                  (db:log :username (db:auth-token->username auth-token)
                          :category "get"
                          :entity_id parsed
                          :details "failure: invalid parameter")
                  "No component parameter provided!")

                 (t
                  (db:log :username (db:auth-token->username auth-token)
                          :category "get"
                          :entity_id parsed
                          :details "success")
                  (format nil "~S"
                          (db:get (db:make-entity :id parsed)
                                  (intern (string-upcase component) :keyword))))))
          (t (setf (hunchentoot:return-code*) hunchentoot:+HTTP-FORBIDDEN+)
             (db:log :username (db:auth-token->username auth-token)
                     :category "get"
                     :entity_id parsed
                     :details "failure: not authorized")
             ""))))

(hunchentoot:define-easy-handler (api-set :uri "/set")
    (auth-token id component value)
  (setf (hunchentoot:content-type*) "text/plain")
  (let ((parsed (parse-id-or-nil id)))
    (cond
      ((db:user-auth-token-valid-p auth-token)
       (cond ((null parsed)
              (setf (hunchentoot:return-code*) hunchentoot:+HTTP-BAD-REQUEST+)
              (db:log :username (db:auth-token->username auth-token)
                      :category "set"
                      :details "failure: invalid id")
              "Invalid id parameter!")

             ((or (null component) (string= component ""))
              (setf (hunchentoot:return-code*) hunchentoot:+HTTP-BAD-REQUEST+)
              (db:log :username (db:auth-token->username auth-token)
                      :category "set"
                      :entity_id parsed
                      :details "failure: invalid parameter")
              "No component parameter provided!")

             (t
              (db:log :username (db:auth-token->username auth-token)
                      :category "set"
                      :entity_id parsed                      
                      :details "success")
              (format
               nil "~S"
               (db:set (db:make-entity :id parsed)
                       (intern (string-upcase component) :keyword)
                       (let ((*read-eval* nil)) (read-from-string value)))))))
      (t (setf (hunchentoot:return-code*) hunchentoot:+HTTP-FORBIDDEN+)
         (db:log :username (db:auth-token->username auth-token)
                 :category "set"
                 :entity_id parsed                      
                 :details "failure: not authorized"))))
  "")

(hunchentoot:define-easy-handler (api-unset :uri "/unset") (auth-token id component)
  (setf (hunchentoot:content-type*) "text/plain")
  (let ((parsed (parse-id-or-nil id)))
    (cond
      ((db:user-auth-token-valid-p auth-token)
       (cond ((null parsed)
              (setf (hunchentoot:return-code*) hunchentoot:+HTTP-BAD-REQUEST+)
              (db:log :username (db:auth-token->username auth-token)
                      :category "unset"
                      :details "failure: invalid id")
              "Invalid id parameter!")

             ((or (null component) (string= component ""))
              (setf (hunchentoot:return-code*) hunchentoot:+HTTP-BAD-REQUEST+)
              (db:log :username (db:auth-token->username auth-token)
                      :category "unset"
                      :entity_id parsed
                      :details "failure: invalid parameter")
              "No component parameter provided!")

             (t
              (db:log :username (db:auth-token->username auth-token)
                      :category "unset"
                      :entity_id parsed
                      :details "success")
              (db:unset (db:make-entity :id parsed)
                        (intern (string-upcase component) :keyword)))))
      (t (setf (hunchentoot:return-code*) hunchentoot:+HTTP-FORBIDDEN+)
         (db:log :username (db:auth-token->username auth-token)
                 :category "unset"
                 :entity_id parsed
                 :details "failure: not authorized"))))
  "")

(hunchentoot:define-easy-handler (api-query :uri "/query") (auth-token components)
  (setf (hunchentoot:content-type*) "text/plain")
  (cond
    ((db:user-auth-token-valid-p auth-token)
     (let ((components-vector (let ((*read-eval* nil))
                                (handler-case (read-from-string components)
                                  (parse-error () :parse-error)))))
       (cond ((not (vectorp components-vector))
              (setf (hunchentoot:return-code*) hunchentoot:+HTTP-BAD-REQUEST+)
              (db:log :username (db:auth-token->username auth-token)
                      :category "query"
                      :details "failure: invalid parameter")
              "Invalid components parameter!")

             (t
              (db:log :username (db:auth-token->username auth-token)
                      :category "query"
                      :details "success")
              (format nil "~S" (db:query components-vector))))))
    (t (setf (hunchentoot:return-code*) hunchentoot:+HTTP-FORBIDDEN+)
       (db:log :username (db:auth-token->username auth-token)
               :category "query"
               :details "failure: not authorized")
       "")))

(hunchentoot:define-easy-handler (api-query-components :uri "/query-components")
    (auth-token components)
  (setf (hunchentoot:content-type*) "text/plain")
  (cond
    ((db:user-auth-token-valid-p auth-token)
     (let ((components-vector (let ((*read-eval* nil))
                                (handler-case (read-from-string components)
                                  (parse-error () :parse-error)))))
       (cond ((not (vectorp components-vector))
              (setf (hunchentoot:return-code*) hunchentoot:+HTTP-BAD-REQUEST+)
              (db:log :username (db:auth-token->username auth-token)
                      :category "query-components"
                      :details "failure: invalid parameter")
              "Invalid components parameter!")

             (t
              (db:log :username (db:auth-token->username auth-token)
                      :category "query-components"
                      :details "success")
              (format nil "~S" (db:query-components components-vector))))))
        (t (setf (hunchentoot:return-code*) hunchentoot:+HTTP-FORBIDDEN+)
           (db:log :username (db:auth-token->username auth-token)
                   :category "query-components"
                   :details "failure: not authorized")
           "")))

(hunchentoot:define-easy-handler (api-get-components :uri "/get-components")
    (auth-token id)
  (setf (hunchentoot:content-type*) "text/plain")
  (let ((parsed (parse-id-or-nil id)))
    (cond
      ((db:user-auth-token-valid-p auth-token)
       (cond ((null parsed)
              (setf (hunchentoot:return-code*) hunchentoot:+HTTP-BAD-REQUEST+)
              (db:log :username (db:auth-token->username auth-token)
                      :category "get-components"
                      :details "failure: invalid id")
              "Invalid id parameter!")

             (t
              (db:log :username (db:auth-token->username auth-token)
                      :category "get-components"
                      :entity_id parsed
                      :details "success")
              (format nil "~S"
                      (db:get-components (db:make-entity :id parsed))))))
      (t (setf (hunchentoot:return-code*) hunchentoot:+HTTP-FORBIDDEN+)
         (db:log :username (db:auth-token->username auth-token)
                 :category "get-components"
                 :entity_id parsed
                 :details "failure: not authorized")
         ""))))

(hunchentoot:define-easy-handler (api-set-components :uri "/set-components")
    (auth-token components-plist)
  (setf (hunchentoot:content-type*) "text/plain")
  (cond
    ((db:user-auth-token-valid-p auth-token)
     (let ((components (let ((*read-eval* nil))
                         (handler-case (read-from-string components-plist)
                           (parse-error () :parse-error)))))
       (cond ((not (listp components))
              (setf (hunchentoot:return-code*) hunchentoot:+HTTP-BAD-REQUEST+)
              (db:log :username (db:auth-token->username auth-token)
                      :category "set-components"
                      :details "failure: invalid parameter")
              "Invalid components-plist parameter!")

             (t
              (db:set-components components)
              (db:log :username (db:auth-token->username auth-token)
                      :category "set-components"
                      :entity_id (getf components :id)
                      :details "success")
              ""))))
    (t (setf (hunchentoot:return-code*) hunchentoot:+HTTP-FORBIDDEN+)
       (db:log :username (db:auth-token->username auth-token)
               :category "set-components"
               :details "failure: not authorized")
       "")))

(hunchentoot:define-easy-handler (api-create :uri "/create") (auth-token)
  (setf (hunchentoot:content-type*) "text/plain")
  (cond
    ((db:user-auth-token-valid-p auth-token)
     (let ((id (db:entity-id (db:create))))
       (db:log :username (db:auth-token->username auth-token)
               :category "create"
               :entity_id id
               :details "success")
       (format nil "~S" id)))
    (t (setf (hunchentoot:return-code*) hunchentoot:+HTTP-FORBIDDEN+)
       (db:log :username (db:auth-token->username auth-token)
               :category "create"
               :details "failure: not authorized"))))

(hunchentoot:define-easy-handler (api-delete :uri "/delete") (auth-token id)
  (setf (hunchentoot:content-type*) "text/plain")
  (let ((parsed (parse-id-or-nil id)))
    (cond

      ((db:user-auth-token-valid-p auth-token)
       (cond ((null parsed)
              (setf (hunchentoot:return-code*) hunchentoot:+HTTP-BAD-REQUEST+)
              (db:log :username (db:auth-token->username auth-token)
                      :category "delete"
                      :entity_id parsed
                      :details "failure: bad id"))
             (t (db:delete (db:make-entity :id parsed))
                (db:log :username (db:auth-token->username auth-token)
                        :category "delete"
                        :entity_id parsed
                        :details "success"))))
      ;; else
      (t (setf (hunchentoot:return-code*) hunchentoot:+HTTP-FORBIDDEN+)
         (db:log :username (db:auth-token->username auth-token)
                 :category "delete"
                 :entity_id parsed
                 :details "failure: not authorized"))))
  "")

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
