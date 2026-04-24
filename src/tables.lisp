(defpackage #:open-orders.tables
  (:use #:cl)
  (:import-from #:defclass-std
                #:defclass/std
                #:class/std)
  (:local-nicknames (#:a #:alexandria)
                    (#:db #:open-orders.db)
                    (#:paths #:open-orders.paths))
  (:export #:user
           #:create-new-user
           #:update-user-password
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
           ;; TODO add exports for the other things
           ))
(in-package #:open-orders.tables)

(defclass/std user ()
  ((name :primary-key t :type string)
   (hash))
  (:metaclass db:sql-table))

(defun create-new-user (database name password)
  (db:database-insert database
                      (make-instance 'user
                                     :name name
                                     :hash (cl-pass:hash password))))

(defun update-user-password (database name password)
  (db:database-update database
                      (make-instance 'user
                                     :name name
                                     :hash (cl-pass:hash password))))

(defclass/std person (db:standard-sql-table)
  ((first-name last-name email phone :type string))
  (:metaclass db:sql-table))

(defclass/std customer (db:standard-sql-table)
  ((primary-contact :type integer :references (person db:id))
   (name :type string))
  (:metaclass db:sql-table))

(defclass/std part (db:standard-sql-table)
  ((part-number :type string)
   (description :type string)
   (revision :type string))
  (:metaclass db:sql-table))

(defclass/std suppliers (db:standard-sql-table)
  ((name :type string)
   (supplies :type list)
   (primary-contact :type integer :references (person db:id)))
  (:metaclass db:sql-table))

(defclass/std material (db:standard-sql-table)
  ((name :type string)
   (categories :type list)
   (suppliers :type list))
  (:metaclass db:sql-table))

(defclass/std open-order (db:standard-sql-table)
  ((customer :type integer :references (customer db:id))
   (purchase-order :type string)
   (line-item :type integer :std 0)
   (part :type integer :references (part db:id))
   (ship-terms :type string :std "PrePay and Add")
   (billing-terms :type string :std "Net 30")
   (ship-notes :type string)
   (material :type integer :references (material db:id)))
  (:metaclass db:sql-table)
  )
