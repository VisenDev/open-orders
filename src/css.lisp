(cl:defpackage #:open-orders.css
  (:use #:cl
        #:open-orders.html-generator
        #:open-orders.templates
        #:open-orders.auth))
(in-package #:open-orders.css)


(defparameter *styles*
  (mapcar (lambda (path) (cons (pathname-name path) (uiop:read-file-string path)))
          (directory "src/css/*.css")))
(defparameter *default-style* "classless")

;; CSS loader handler
(hunchentoot:define-easy-handler (css :uri "/css") (style)
  (setf (hunchentoot:content-type*) "text/css")
  (if (and style (not (string= style "")))
      (cdr (assoc style *styles* :test #'string=))
      (cdr (assoc *default-style* *styles* :test #'string=))))

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
      ;; (input (:type "submit" :value "Select"))
      (html-table ()
        (mapcar (lambda (name)
                  (tr ()
                    (td ()
                      (button (:name "style"
                               :value name
                               :type "submit"
                               :action "submit")
                        (if (string= (hunchentoot:cookie-in *css-style-cookie*)
                                     name)
                            (format nil "<i><b><strong>~a</strong></b></i>" name)
                            name)))))
                (mapcar #'car *styles*))))))

(pushnew (make-tab :name "[theme]" :url "/select-theme")
         *toplevel-tabs*
         :test #'equalp)
