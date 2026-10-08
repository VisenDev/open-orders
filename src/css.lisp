(cl:defpackage #:open-orders.css
  (:use #:cl
        #:open-orders.html-generator
        #:open-orders.templates
        #:open-orders.auth
        #:open-orders.serve))
(in-package #:open-orders.css)

(defparameter *styles*
  (mapcar #'pathname-name (directory "src/css/*.css")))
(static-serve-directory "/css/" "src/css" "text/css")

(hunchentoot:define-easy-handler (set-css :uri "/set-css") (redirect-url style)
  (hunchentoot:set-cookie *css-style-cookie* :value style)
  (hunchentoot:redirect redirect-url))

(hunchentoot:define-easy-handler (select-theme :uri "/select-theme") ()
  (with-internal-page
    (hr ())
    (form (:action "/set-css")
      (input (:type "hidden"
              :name "redirect-url"
              :value (hunchentoot:request-uri*)))
      (mapcar (lambda (name)
                (button (:name "style"
                         :value name
                         :type "submit"
                         :action "submit")
                  (if (string= (or (hunchentoot:cookie-in *css-style-cookie*) "")
                               name)
                      (format nil "<i><b><strong>~a</strong></b></i>" name)
                      name)))
              *styles*))))

(pushnew (make-tab :name "[theme]" :url "/select-theme")
         *toplevel-tabs*
         :test #'equalp)
