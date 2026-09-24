(cl:defpackage #:open-orders.main
  (:use #:cl
        #:open-orders.html-generator
        #:open-orders.database
        #:open-orders.derive-page
        ;; #:open-orders.sql-table
        ;; #:open-orders.tables
        #:open-orders.templates
        #:open-orders.auth
        )
  (:import-from #:url-rewrite
                #:url-encode)
  (:export
   #:main))
(in-package #:open-orders.main)

(defvar *acceptor* nil)

(define-table customer
    ((field name
            :type string :initform "lorem ipsum"
            :metadata (:show-in-list-view-p t))
     (field primary-contact-name
            :type string :initform ""
            :metadata (:show-in-list-view-p t))
     (field (email phone)
            :type string :initform ""
            :metadata (:show-in-list-view-p t))))

(define-table order
    ((field po-number :type string :initform "lorem ipsum"
                      :metadata (:show-in-list-view-p t))
     (field customer :type string :initform ""
                     :metadata (:show-in-list-view-p t))
     (field customer-id :type integer :initform -1)
     (field part-number :type string :initform ""
                        :metadata (:show-in-list-view-p t))))

(define-table inventory
  ((field (part-number location note last-updated)
          :type string :initform ""
          :metadata (:show-in-list-view-p t))))

(define-table employee
  ((field (first-name last-name birthday phone email date-hired)
          :type string :initform ""
          :metadata (:show-in-list-view-p t))))

(define-table purchase-order
  ((field (code company description date-placed)
          :type string :initform ""
          :metadata (:show-in-list-view-p t))))

(defmacro derive-all-pages (table-name)
  `(progn
     (derive-list-page-from-table ,table-name)
     (derive-new-page-from-table ,table-name)
     (derive-save-page-from-table ,table-name)
     (derive-edit-page-from-table ,table-name)))

(pushnew (make-tab :name "<i>[logout]</i>" :url "/logout")
         *toplevel-tabs*
         :test #'equalp)
(derive-all-pages customer)
(derive-all-pages inventory)
(derive-all-pages employee)
(derive-all-pages purchase-order)
(derive-all-pages order)


;; Derivations
;; (derive-pages-from-table order (save-url order-value ())
;;   (with-internal-page
;;     (hr ())
;;     (p () "You've reached the edit page!")
;;     (p () (format nil "~a" order-value))))

(hunchentoot:define-easy-handler (home :uri "/") ()
  (hunchentoot:redirect (table-url (find-table 'order) "list")))

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
