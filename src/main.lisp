(defpackage #:open-orders.main
  (:use #:cl
        #:open-orders.fn
        #:open-orders.html-generator
        #:open-orders.database
        #:open-orders.derive-page
        #:open-orders.templates
        #:open-orders.auth
        #:open-orders.tables
        )
  (:import-from #:url-rewrite
                #:url-encode)
  (:export
   #:main))
(in-package #:open-orders.main)


;;;; IMPORTANT
;;;; Setting catch-errors-p to nil enables the lisp debugger
;;;; This should be set to t when running in production
(setf hunchentoot:*catch-errors-p* nil)

(defvar *acceptor* nil)

(defun derive-all-pages (table-name)
  (derive-list-page-from-table table-name)
  (derive-new-page-from-table table-name)
  (derive-delete-page-from-table table-name)
  (derive-save-page-from-table table-name)
  (derive-edit-page-from-table table-name)
  (derive-view-reference-page-from-table table-name)
  (derive-set-field-page-from-table table-name))

(pushnew (make-tab :name "<i>[logout]</i>" :url "/logout")
         *toplevel-tabs*
         :test #'equalp)
(derive-all-pages 'customer)
(derive-all-pages 'inventory)
(derive-all-pages 'employee)
(derive-all-pages 'purchase-order)
(derive-all-pages 'po-details)

(hunchentoot:define-easy-handler (home :uri "/") ()
  (hunchentoot:redirect (table-url (find-table 'po-details) "list")))

(defun start ()
  (setf *database-path* (asdf:system-relative-pathname "open-orders" "database/"))
  (setf *acceptor* (make-instance 'hunchentoot:easy-acceptor :port 8000))
  (hunchentoot:start *acceptor*))

(defun stop ()
  (when *acceptor*
    (hunchentoot:stop *acceptor*)
    (setf *acceptor* nil)))

(defun main ()
  (start)
  (unwind-protect (loop (sleep 1))
    (stop)))
