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
         (db:log
          nil "login_success" "(:ip ~S :username ~S)"
          (hunchentoot:real-remote-addr) username)
         
         (format nil "~S" (db:user-get-auth-token username password)))

        (t
         (db:log
          nil "login_failure" "(:ip ~S :username ~S)"
          (hunchentoot:real-remote-addr) username)
         (setf (hunchentoot:return-code*)
               hunchentoot:+HTTP-AUTHORIZATION-REQUIRED+)
         "")))

(hunchentoot:define-easy-handler (api-get :uri "/get") (auth-token id component)
  (setf (hunchentoot:content-type*) "text/plain")
  (cond ((db:user-auth-token-valid-p auth-token)
         (let ((parsed (parse-id-or-nil id)))
           (cond ((null parsed)
                  (setf (hunchentoot:return-code*) hunchentoot:+HTTP-BAD-REQUEST+)
                  "Invalid id parameter!")

                 ((or (null component) (string= component ""))
                  (setf (hunchentoot:return-code*) hunchentoot:+HTTP-BAD-REQUEST+)
                  "No component parameter provided!")

                 (t
                  (format nil "~S"
                          (db:get (db:make-entity :id parsed)
                                  (intern (string-upcase component) :keyword)))))))
        (t (setf (hunchentoot:return-code*) hunchentoot:+HTTP-FORBIDDEN+)
           "")))

(hunchentoot:define-easy-handler (api-set :uri "/set")
    (auth-token id component value)
  (setf (hunchentoot:content-type*) "text/plain")
  (cond
    ((db:user-auth-token-valid-p auth-token)
     (let ((parsed (parse-id-or-nil id)))
       (cond ((null parsed)
              (setf (hunchentoot:return-code*) hunchentoot:+HTTP-BAD-REQUEST+)
              "Invalid id parameter!")

             ((or (null component) (string= component ""))
              (setf (hunchentoot:return-code*) hunchentoot:+HTTP-BAD-REQUEST+)
              "No component parameter provided!")

             (t
              (format
               nil "~S"
               (db:set (db:make-entity :id parsed)
                       (intern (string-upcase component) :keyword)
                       (let ((*read-eval* nil)) (read-from-string value))))))))
    (t (setf (hunchentoot:return-code*) hunchentoot:+HTTP-FORBIDDEN+)))
  "")

(hunchentoot:define-easy-handler (api-unset :uri "/unset") (auth-token id component)
  (setf (hunchentoot:content-type*) "text/plain")
  (cond
    ((db:user-auth-token-valid-p auth-token)
     (let ((parsed (parse-id-or-nil id)))
       (cond ((null parsed)
              (setf (hunchentoot:return-code*) hunchentoot:+HTTP-BAD-REQUEST+)
              "Invalid id parameter!")

             ((or (null component) (string= component ""))
              (setf (hunchentoot:return-code*) hunchentoot:+HTTP-BAD-REQUEST+)
              "No component parameter provided!")

             (t
              (db:unset (db:make-entity :id parsed)
                        (intern (string-upcase component) :keyword))))))
    (t (setf (hunchentoot:return-code*) hunchentoot:+HTTP-FORBIDDEN+)))
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
              "Invalid components parameter!")

             (t
              (format nil "~S" (db:query components-vector))))))
    (t (setf (hunchentoot:return-code*) hunchentoot:+HTTP-FORBIDDEN+)
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
              "Invalid components parameter!")

             (t
              (format nil "~S" (db:query-components components-vector))))))
        (t (setf (hunchentoot:return-code*) hunchentoot:+HTTP-FORBIDDEN+)
           "")))

(hunchentoot:define-easy-handler (api-get-components :uri "/get-components")
    (auth-token id)
  (setf (hunchentoot:content-type*) "text/plain")
  (cond
    ((db:user-auth-token-valid-p auth-token)
     (let ((parsed (parse-id-or-nil id)))
       (cond ((null parsed)
              (setf (hunchentoot:return-code*) hunchentoot:+HTTP-BAD-REQUEST+)
              "Invalid id parameter!")

             (t
              (format nil "~S"
                      (db:get-components (db:make-entity :id parsed)))))))
        (t (setf (hunchentoot:return-code*) hunchentoot:+HTTP-FORBIDDEN+)
           "")))

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
              "Invalid components-plist parameter!")

             (t
              (db:set-components components)
              ""))))
        (t (setf (hunchentoot:return-code*) hunchentoot:+HTTP-FORBIDDEN+)
           "")))

(hunchentoot:define-easy-handler (api-create :uri "/create") (auth-token)
  (setf (hunchentoot:content-type*) "text/plain")
  (cond
    ((db:user-auth-token-valid-p auth-token)
     (let ((id (db:entity-id (db:create))))
       (db:log
        auth-token "create" "(:ip ~S :new-id ~S)"
        (hunchentoot:real-remote-addr) id)
       (format nil "~S" id)))
    (t (setf (hunchentoot:return-code*) hunchentoot:+HTTP-FORBIDDEN+)
       (db:log auth-token "unauthorized"
               "(:ip ~S :action \"create\")"
               (hunchentoot:real-remote-addr)))))

(hunchentoot:define-easy-handler (api-delete :uri "/delete") (auth-token id)
  (setf (hunchentoot:content-type*) "text/plain")
  (cond

    ((db:user-auth-token-valid-p auth-token)
     (let ((parsed (parse-id-or-nil id)))
       (cond ((null parsed)
              (setf (hunchentoot:return-code*) hunchentoot:+HTTP-BAD-REQUEST+)
              "Invalid id parameter!")
             (t (db:delete (db:make-entity :id parsed)))))
     (db:log auth-token "delete" "(:ip ~S :id ~S)"
             (hunchentoot:real-remote-addr) id))

    ;; else
    (t (setf (hunchentoot:return-code*) hunchentoot:+HTTP-FORBIDDEN+)
       (db:log auth-token "unauthorized"
               "(:ip ~S :id ~S :action \"delete\")"
               (hunchentoot:real-remote-addr) id)))
  "")

(let (acceptor)
  (defun start ()
    (db:connect)
    (db:log nil "system" "connect")
    (when acceptor
      (hunchentoot:stop acceptor))
    (setf acceptor (make-instance 'hunchentoot:easy-acceptor :port 8888))

    (hunchentoot:start acceptor))

  (defun stop ()
    (db:disconnect)
    (db:log nil "system" "disconnect")
    (when acceptor
      (hunchentoot:stop acceptor))
    (setf acceptor nil))

  (defun main ()
    (start)
    (unwind-protect (loop (sleep 1))
      (stop))))
