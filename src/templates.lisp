(defpackage #:open-orders.templates
  (:use #:cl
        #:open-orders.html-generator)
  (:export
   #:with-page
   #:*toplevel-tabs*
   #:tab
   #:make-tab
   #:tab-p
   #:copy-tab
   #:tab-name
   #:tab-url
   #:insert-toplevel-tabs))
(in-package #:open-orders.templates)

(defstruct tab name url)
(defparameter *toplevel-tabs* nil)

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


(defun insert-toplevel-tabs ()
  (html-table ()
    (tr ()
       (mapcar (lambda (tab)
                 (td ()
                   (a (:href (tab-url tab))
                     (tab-name tab))))
                 *toplevel-tabs*))))
