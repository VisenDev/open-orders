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
                 (db:get (db:make-entity :id (parse-integer id))
                         (intern (string-upcase component) :keyword))))))

(hunchentoot:define-easy-handler (api-get-components :uri "/get-components") (id)
  (setf (hunchentoot:content-type*) "text/plain")
  (cond ((or (null id) (string= id ""))
         (setf (hunchentoot:return-code*) hunchentoot:+HTTP-BAD-REQUEST+)
         "No id parameter provided!")

        (t
         (format nil "~S"
                 (db:get-components (db:make-entity :id (parse-integer id)))))))




(defun start ()
  (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 8888)))
