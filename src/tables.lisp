(defpackage #:open-orders.tables
  (:use #:cl #:open-orders.sql-table)
  (:import-from #:defclass-std
                #:defclass/std)
  (:export #:user
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
           #:authentication-token-timestamp
           #:contactable-mixin
           #:contact-first-name
           #:contact-last-name
           #:contact-email
           #:contact-phone
           #:connection
           #:db
           #:user-create-new
           #:scheduled-shipment
           #:make-scheduled-shipment
           #:scheduled-shipment-p
           #:copy-scheduled-shipment
           #:scheduled-shipment-date
           #:scheduled-shipment-quota
           #:scheduled-shipment-amount
           #:scheduled-shipment-completed-p
           #:open-order-deadline
           #:open-orders-table
           #:select-slot))
(in-package #:open-orders.tables)

(defclass autodefined-table () ())
(defclass open-orders-table ()
  ((id :accessor id
       :type integer
       :primary-key t
       :autoincrement t
       :initform nil
       :initarg :id)
   (notes :accessor notes
          :type list
          :initform nil
          :initarg :notes))
  (:metaclass sql-table))

(defclass/std user (autodefined-table)
  ((name :type string :primary-key t)
   (hash authentication-token :type string)
   (authentication-token-timestamp :type integer))
  (:metaclass sql-table))





;; (defclass/std person (open-orders-table)
;;   ((first-name last-name email phone :type string))
;;   (:metaclass sql-table))

(defclass/std contactable-mixin (autodefined-table)
  ((contact-first-name contact-last-name contact-email contact-phone
                       :type string :std ""))
  (:metaclass sql-table))

(defclass/std customer (open-orders-table contactable-mixin autodefined-table)
  ((name :type string))
  (:metaclass sql-table))

(defstruct scheduled-shipment
  (date (get-universal-time) :type integer)
  (quota 0 :type integer)
  (amount 0 :type integer))

(declaim (ftype (function (scheduled-shipment) boolean)
                scheduled-shipment-completed-p))
(defun scheduled-shipment-completed-p (shipment)
  (>= (scheduled-shipment-amount shipment)
      (scheduled-shipment-quota shipment)))

(defclass/std part (open-orders-table autodefined-table)
  ((part-number :type string)
   (description :type string)
   (revision :type string)
   (inventory-count :type integer)
   (inventory-location :type integer))
  (:metaclass sql-table))

(defclass/std suppliers (open-orders-table contactable-mixin autodefined-table)
  ((name :type string)
   (supplies :type list))
  (:metaclass sql-table))

(defclass/std material (open-orders-table autodefined-table)
  ((name :type string)
   (categories :type list)
   (suppliers :type list))
  (:metaclass sql-table))

(defclass/std open-order (open-orders-table autodefined-table)
  ((customer :type integer :references (customer id))
   (purchase-order :type string)
   (line-item :type integer :std 0)
   (part :type integer :references (part id))
   (ship-terms :type string :std "PrePay and Add")
   (billing-terms :type string :std "Net 30")
   (ship-notes :type string)
   (material :type integer :references (material id))
   (run-status :type string)
   (scheduled-shipments :type list :doc "list of scheduled-shipment"))
  (:metaclass sql-table))

(declaim (ftype (function (open-order) integer) open-order-deadline))
(defun open-order-deadline (open-order)
  (let ((filtered (remove-if #'scheduled-shipment-completed-p
                             (scheduled-shipments open-order))))
    (loop :for shipment :in filtered
          :minimize (scheduled-shipment-date shipment))))


(declaim (ftype (function (symbol (or string integer) symbol) t)
                select-slot))
(defun select-slot (classname id slotname)
  "Looks up the object in the database and accesses slotname, returns nil otherwise."
  (let* ((parsed-id (if (stringp id) (parse-integer id)
                        id))
         (obj (select classname 'id parsed-id)))
    (when obj
      (when (slot-boundp obj slotname)
        (slot-value obj slotname)))))

;;; Utils
(defun database-connect ()
  (unless *database-handle*
    (let ((db (dbi:connect :sqlite3 :database-name "open-orders.sqlite3")))
      (setf *database-handle* db)
      (dolist (class
               (closer-mop:class-direct-subclasses (find-class 'autodefined-table)))
        (create (class-name class) :if-not-exists t)))))

(defun database-disconnect ()
  (when *database-handle*
    (dbi:disconnect *database-handle*)
    (setf *database-handle* nil)))

(defun %nuke-tables ()
  (database-connect)
  (dolist (class
           (closer-mop:class-direct-subclasses (find-class 'autodefined-table)))
    (drop (class-name class) :if-exists t)))

(defun user-create-new (name password)
  (database-connect)
  (insert (make-instance 'user
                         :name name
                         :hash (cl-pass:hash password))))
