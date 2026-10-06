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
   #:insert-toplevel-tabs
   #:mobile-browser-p
   #:*css-style-cookie*))
(in-package #:open-orders.templates)

(defstruct tab name url)
(defvar *toplevel-tabs* nil)

(defun mobile-browser-p ()
  (let ((user-agent (hunchentoot:header-in* :user-agent)))
    (and user-agent
         (or (search "Android" user-agent)
             (search "iPhone" user-agent)
             (search "iPad" user-agent)
             (search "Mobile" user-agent)))))

(defparameter *css-style-cookie* "CSS_STYLE")
(defun get-css-href ()
  (format nil "/css?style=~a"
          (or (hunchentoot:cookie-in *css-style-cookie*) "")))

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
           (link (:href (get-css-href)
                  :rel "stylesheet"))
           (link (:rel "manifest"
                  :href "/manifest.json")))
         (body ()
           ,@body
           )
         (script (:id "PWA-registration")
           "if (\"serviceWorker\" in navigator) { navigator.serviceWorker.register(\"/service-worker.js\"); }")))))



(defun insert-toplevel-tabs ()
  (html-table ()
    (if (mobile-browser-p)
        (mapcar (lambda (tab)
                  (tr ()
                    (td ()
                      (a (:href (tab-url tab))
                        (tab-name tab)))))
                *toplevel-tabs*)
        (tr ()
          (mapcar (lambda (tab)
                    (td ()
                      (a (:href (tab-url tab))
                        (tab-name tab))))
                  *toplevel-tabs*)))))
