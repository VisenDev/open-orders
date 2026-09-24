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

(derive-list-page-from-table customer)
(derive-new-page-from-table customer)
(derive-save-page-from-table customer)
(derive-edit-page-from-table customer)

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
