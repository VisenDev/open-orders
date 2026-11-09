(defpackage #:open-orders.server.api
  (:use #:cl)
  (:local-nicknames (#:db #:open-orders.server.db)))
(in-package #:open-orders.server.api)

(hunchentoot:define-easy-handler (api-get :uri "/get") (id component)
  (setf (hunchentoot:content-type*) "text/plain")
  (cond ((or (null id) (string= id ""))
         (setf (hunchentoot:return-code*) hunchentoot:+HTTP-BAD-REQUEST+)
         "No id parameter provided!")

        ((or (null component) (string= component ""))
         (setf (hunchentoot:return-code*) hunchentoot:+HTTP-BAD-REQUEST+)
         "No component parameter provided!")

        (t
         (format nil "~S"
                 (db:get (db:make-entity :id (print (parse-integer id)))
                         (intern component '#:keyword))))))




(defun start ()
  (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 8888)))
