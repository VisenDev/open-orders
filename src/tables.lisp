(defpackage #:open-orders.tables
  (:use #:cl)
  (:import-from #:defclass-std
                #:defclass/std
                #:class/std)
  (:local-nicknames (#:a #:alexandria)
                    ;; (#:db #:open-orders.db)
                    (#:sql #:open-orders.sql-table)
                    (#:paths #:open-orders.paths)
                    (#:mop #:closer-mop))
  (:export #:user
           #:user-create-new
           #:user-update-password
           #:person
           #:customer
           #:part
           #:supplier
           #:material
           #:open-order
           #:name
           #:hash
           #:first-name
           #:last-name
           #:email
           #:phone
           #:notes
           #:id
           ;; WOAH I CAN USE SLIME-EXPORT-CLASS TO EXPORT CLASS ACCESSORS
           #:purchase-order
           #:line-item
           #:ship-terms
           #:billing-terms
           #:ship-notes
           #:primary-contact
           #:part-number
           #:description
           #:revision
           #:suppliers
           #:supplies
           #:categories
           #:with-database
           #:database-disconnect
           #:database-connect
           #:authentication-token
           #:authentication-token-timestamp))
(in-package #:open-orders.tables)

(defclass autodefined-table () ())
(defclass open-orders-table (autodefined-table)
  ((id :accessor id
       :type integer
       :primary-key t
       :autoincrement t
       :initform nil
       :initarg :id)
   (notes :accessor notes
          :type string
          :initform nil
          :initarg :notes))
  (:metaclass sql:sql-table))

(defclass/std user (autodefined-table)
  ((name :type string :primary-key t)
   (hash authentication-token :type string)
   (authentication-token-timestamp :type integer))
  (:metaclass sql:sql-table))

(defun user-create-new (database name password)
  (sql:exec-insert
      (make-instance 'user
                     :name name
                     :hash (cl-pass:hash password))
      database))

(defun user-update-password (database name password)
  (sql:exec-update
   (make-instance 'user
                  :name name
                  :hash (cl-pass:hash password))
   database))



;; (defclass/std person (open-orders-table)
;;   ((first-name last-name email phone :type string))
;;   (:metaclass sql:sql-table))

(defclass/std customer (open-orders-table)
  ((email phone :type string)
   (name :type string))
  (:metaclass sql:sql-table))

(defclass/std part (open-orders-table)
  ((part-number :type string)
   (description :type string)
   (revision :type string))
  (:metaclass sql:sql-table))

(defclass/std suppliers (open-orders-table)
  ((name :type string)
   (supplies :type list)
   (primary-contact :type integer :references (person id)))
  (:metaclass sql:sql-table))

(defclass/std material (open-orders-table)
  ((name :type string)
   (categories :type list)
   (suppliers :type list))
  (:metaclass sql:sql-table))

(defclass/std open-order (open-orders-table)
  ((customer :type integer :references (customer id))
   (purchase-order :type string)
   (line-item :type integer :std 0)
   (part :type integer :references (part id))
   (ship-terms :type string :std "PrePay and Add")
   (billing-terms :type string :std "Net 30")
   (ship-notes :type string)
   (material :type integer :references (material id)))
  (:metaclass sql:sql-table))

(defun database-connect ()
  (let ((db (dbi:connect :sqlite3 :database-name paths:*db-path*)))

    (dolist (class (mop:class-direct-subclasses (find-class 'autodefined-table)))
      (sql:exec-create (class-name class) t db))
    db))

(defun database-disconnect (db)
  (dbi:disconnect db))

(defun %nuke-tables (db)
  (dolist (class (mop:class-direct-subclasses (find-class 'autodefined-table)))
      (sql:exec-drop (class-name class) t db)))
