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
(defparameter *css-style-default* "classless")

(defun call-with-page (body-callback)
  (progn
    (setf (hunchentoot:content-type*) "text/html")
    (doctype ()
      (html ()
        (head ()
          (title () "Open Orders")
          (meta (:charset "utf-8"))
          (meta (:name "viewport"
                 :content "width=device-width, initial-scale=1"))
          (link (:href (format nil "/css/~a.css"
                               (or (hunchentoot:cookie-in *css-style-cookie*)
                                   *css-style-default*))
                 :rel "stylesheet"))
          (link (:rel "manifest"
                 :href "/manifest.json"))
          (script (:src "/js/save-scroll.js")))
        (body ()
          (funcall body-callback)
          (script (:src "/js/pwa-registration.js"))
          (script (:src "/js/keyboard-navigation.js?v=5")))))))

(defmacro with-page (&body body)
  `(call-with-page
    (lambda ()
      (list ,@body))))

(defun insert-toplevel-tabs ()
  (html-table (:class "toplevel-link-table selectable")
    (if (mobile-browser-p)
        (mapcar (lambda (tab)
                  (tr (:class "toplevel-mobile-link-row")
                    (td (:class "toplevel-mobile-link-data")
                      (a (:class "selectable toplevel-link"
                          :href (tab-url tab))
                        (tab-name tab)))))
                *toplevel-tabs*)
        (tr (:class "toplevel-link-row")
          (mapcar (lambda (tab)
                    (td (:class "toplevel-link-data")
                      (a (:href (tab-url tab)
                          :class "toplevel-link selectable")
                        (tab-name tab))))
                  *toplevel-tabs*)))))



