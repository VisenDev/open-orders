(defpackage #:open-orders.tables
  (:use #:cl)
  (:import-from #:defclass-std
                #:defclass/std
                #:class/std)
  (:local-nicknames (#:a #:alexandria)
                    (#:db #:open-orders.db)
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


(defclass open-orders-table () () (:metaclass db:sql-table))

(defclass/std user (open-orders-table)
  ((name :primary-key t :type string)
   (hash authentication-token :type string)
   (authentication-token-timestamp :type integer))
  (:metaclass db:sql-table))

(defun user-create-new (database name password)
  (db:database-insert database
                      (make-instance 'user
                                     :name name
                                     :hash (cl-pass:hash password))))

(defun user-update-password (database name password)
  (db:database-update database
                      (make-instance 'user
                                     :name name
                                     :hash (cl-pass:hash password))))



(defclass/std person (open-orders-table db:standard-sql-table)
  ((first-name last-name email phone :type string))
  (:metaclass db:sql-table))

(defclass/std customer (open-orders-table db:standard-sql-table)
  ((primary-contact :type integer :references (person db:id))
   (name :type string))
  (:metaclass db:sql-table))

(defclass/std part (open-orders-table db:standard-sql-table)
  ((part-number :type string)
   (description :type string)
   (revision :type string))
  (:metaclass db:sql-table))

(defclass/std suppliers (open-orders-table db:standard-sql-table)
  ((name :type string)
   (supplies :type list)
   (primary-contact :type integer :references (person db:id)))
  (:metaclass db:sql-table))

(defclass/std material (open-orders-table db:standard-sql-table)
  ((name :type string)
   (categories :type list)
   (suppliers :type list))
  (:metaclass db:sql-table))

(defclass/std open-order (open-orders-table db:standard-sql-table)
  ((customer :type integer :references (customer db:id))
   (purchase-order :type string)
   (line-item :type integer :std 0)
   (part :type integer :references (part db:id))
   (ship-terms :type string :std "PrePay and Add")
   (billing-terms :type string :std "Net 30")
   (ship-notes :type string)
   (material :type integer :references (material db:id)))
  (:metaclass db:sql-table))


(defun database-connect ()
  (let ((db (dbi:connect :sqlite3 :database-name paths:*db-path*)))
    (db:database-initialize db)

    (dolist (class (mop:class-direct-subclasses (find-class 'open-orders-table)))
      (db:database-create-table db (class-name class) :if-not-exists t))
    db))

(defun database-disconnect (db)
  (dbi:disconnect db))

(defmacro with-database (var &body body)
  `(let ((,var (database-connect)))
     (unwind-protect
          (progn ,@body)
       (database-disconnect ,var))))

(defun %nuke-tables (db)
    (dolist (class (mop:class-direct-subclasses (find-class 'open-orders-table)))
      (db:database-drop-table db (class-name class) :if-exists t)))
