(defpackage #:open-orders.mop
  (:use #:cl)
  (:local-nicknames (#:db #:open-orders.database)
                    (#:a #:alexandria)
                    (#:mop #:closer-mop)))
(in-package #:open-orders.mop)

(defun lisp-identifier->sql-identifier (ident)
  (intern (map 'string (lambda (ch)
                    (if (alphanumericp ch) ch #\_))
                  (symbol-name ident))))

(defclass sql-table (standard-class)
  ((sql-name :accessor sql-name)))

(defmethod mop:finalize-inheritance :after ((class sql-table))
  (setf (sql-name class)
        (lisp-identifier->sql-identifier (class-name class))))

(defclass sql-table-effective-slot-definition (mop:standard-effective-slot-definition)
  ((primary-key :accessor primary-key :initform nil)
   (references :accessor references :initform nil)
   (autoincrement :accessor autoincrement :initform nil)
   (sql-name :accessor sql-name :initform nil)))

(defclass sql-table-direct-slot-definition (mop:standard-direct-slot-definition)
  ((primary-key :accessor primary-key :initform nil
                :initarg :primary-key :initarg :primarykey)
   (references :accessor references :initform nil :initarg :references)
   (autoincrement :accessor autoincrement :initform nil
                  :initarg :autoincrement :initarg :auto-increment)
   ))

(defmethod mop:validate-superclass ((class sql-table) (superclass standard-class))
  t)

(defmethod mop:effective-slot-definition-class ((class sql-table) &rest initargs)
  (declare (ignore initargs))
  (find-class 'sql-table-effective-slot-definition))

(defmethod mop:direct-slot-definition-class ((class sql-table) &rest initargs)
  (declare (ignore initargs))
  (find-class 'sql-table-direct-slot-definition))

(defmethod mop:compute-effective-slot-definition ((class sql-table) name direct-slots)
  (let ((eslot (call-next-method)))
    (setf (primary-key eslot)
          (some #'primary-key direct-slots))
    (setf (references eslot)
          (some #'references direct-slots))
    (setf (autoincrement eslot)
          (some #'autoincrement direct-slots))
    (setf (sql-name eslot)
          (lisp-identifier->sql-identifier name))
    eslot))

(defmethod mop:slot-definition-allocation ((slotd sql-table-effective-slot-definition))
  (call-next-method))

(defmethod mop:slot-definition-allocation ((slotd sql-table-direct-slot-definition))
  (call-next-method))

(defclass standard-sql-table ()
  ((id :accessor id :primary-key t :auto-increment t :initform nil :initarg :id))
  (:metaclass sql-table))



(defgeneric create-table (class database))
(defmethod create-table ((class sql-table) database)
  (format nil "CREATE TABLE ~a" (sql-name class)))


(defclass person (standard-sql-table)
  ((first-name :accessor first-name)
   (last-name :accessor last-name)
   (email :accessor email)
   (phone :accessor phone))
  (:metaclass sql-table))

(defclass company (standard-sql-table)
  ((primary-contact :accessor primary-contact :references (person id)))
  (:metaclass sql-table))


(defparameter *bob-jones* (make-instance 'company))

(print (references (second (mop:class-slots (class-of *person*)))))
