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
  )

(hunchentoot:define-easy-handler (api-get :uri "/get") (id component)
  (setf (hunchentoot:content-type*) "text/plain")
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

(hunchentoot:define-easy-handler (api-set :uri "/set") (id component value)
  (setf (hunchentoot:content-type*) "text/plain")
  (let ((parsed (parse-id-or-nil id)))
    (cond ((null parsed)
           (setf (hunchentoot:return-code*) hunchentoot:+HTTP-BAD-REQUEST+)
           "Invalid id parameter!")

          ((or (null component) (string= component ""))
           (setf (hunchentoot:return-code*) hunchentoot:+HTTP-BAD-REQUEST+)
           "No component parameter provided!")

          (t
           (format nil "~S"
                   (db:set (db:make-entity :id parsed)
                           (intern (string-upcase component) :keyword)
                           (let ((*read-eval* nil)) (read-from-string value)))))))
  "")

(hunchentoot:define-easy-handler (api-unset :uri "/unset") (id component)
  (setf (hunchentoot:content-type*) "text/plain")
  (let ((parsed (parse-id-or-nil id)))
    (cond ((null parsed)
           (setf (hunchentoot:return-code*) hunchentoot:+HTTP-BAD-REQUEST+)
           "Invalid id parameter!")

          ((or (null component) (string= component ""))
           (setf (hunchentoot:return-code*) hunchentoot:+HTTP-BAD-REQUEST+)
           "No component parameter provided!")

          (t
           (db:unset (db:make-entity :id parsed)
                     (intern (string-upcase component) :keyword)))))
  "")

(hunchentoot:define-easy-handler (api-query :uri "/query") (components)
  (setf (hunchentoot:content-type*) "text/plain")
  (let ((components-vector (let ((*read-eval* nil))
                             (handler-case (read-from-string components)
                               (parse-error () :parse-error)))))
    (cond ((not (vectorp components-vector))
           (setf (hunchentoot:return-code*) hunchentoot:+HTTP-BAD-REQUEST+)
           "Invalid components parameter!")

          (t
           (format nil "~S" (db:query components-vector))))))

(hunchentoot:define-easy-handler (api-query-components :uri "/query-components")
    (components)
  (setf (hunchentoot:content-type*) "text/plain")
  (let ((components-vector (let ((*read-eval* nil))
                             (handler-case (read-from-string components)
                               (parse-error () :parse-error)))))
    (cond ((not (vectorp components-vector))
           (setf (hunchentoot:return-code*) hunchentoot:+HTTP-BAD-REQUEST+)
           "Invalid components parameter!")

          (t
           (format nil "~S" (db:query-components components-vector))))))

(hunchentoot:define-easy-handler (api-get-components :uri "/get-components")
    (id)
  (setf (hunchentoot:content-type*) "text/plain")
  (let ((parsed (parse-id-or-nil id)))
    (cond ((null parsed)
           (setf (hunchentoot:return-code*) hunchentoot:+HTTP-BAD-REQUEST+)
           "Invalid id parameter!")

          (t
           (format nil "~S"
                   (db:get-components (db:make-entity :id parsed)))))))

(hunchentoot:define-easy-handler (api-set-components :uri "/set-components")
    (components-plist)
  (setf (hunchentoot:content-type*) "text/plain")
  (let ((components (let ((*read-eval* nil))
                      (handler-case (read-from-string components-plist)
                        (parse-error () :parse-error)))))
    (cond ((not (listp components))
           (setf (hunchentoot:return-code*) hunchentoot:+HTTP-BAD-REQUEST+)
           "Invalid components-plist parameter!")

          (t
           (db:set-components components)
           ""))))

(hunchentoot:define-easy-handler (api-create :uri "/create") ()
  (setf (hunchentoot:content-type*) "text/plain")
  (format nil "~S" (db:entity-id (db:create))))

(hunchentoot:define-easy-handler (api-delete :uri "/delete") (id)
  (setf (hunchentoot:content-type*) "text/plain")
  (let ((parsed (parse-id-or-nil id)))
    (cond ((null parsed)
           (setf (hunchentoot:return-code*) hunchentoot:+HTTP-BAD-REQUEST+)
           "Invalid id parameter!")
          (t (db:delete (db:make-entity :id parsed)))))
  "")

(defun start ()
  (db:connect)
  (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 8888)))

(defun stop ()
  (db:disconnect)
  (hunchentoot:stop hunchentoot:*acceptor*))

(defun main ()
  (start)
  (unwind-protect (loop (sleep 1))
    (stop)))
