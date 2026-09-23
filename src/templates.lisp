(defpackage #:open-orders.templates
  (:use #:cl
        #:open-orders.html-generator)
  (:export
   #:with-page))
(in-package #:open-orders.templates)

;; CSS loader handler
(hunchentoot:define-easy-handler (css :uri "/orders.css") ()
  (setf (hunchentoot:content-type*) "text/css")
  #.(uiop:read-file-string (asdf:system-relative-pathname "open-orders"
                                                          "src/orders.css")))

(defmacro with-page (&body body)
  `(progn
     (setf (hunchentoot:content-type*) "text/html")
     (doctype ()
       (html ()
         (head ()
           (title () "Open Orders")
           (meta (:charset "utf-8"))
           (meta (:name "viewport"
                  :content "width=device-width, initial-scale=1"))
           (link (:href "/orders.css" :rel "stylesheet")))
         (body ()
           ,@body)))))


