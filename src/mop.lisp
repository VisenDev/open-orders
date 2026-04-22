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
  ((id :accessor id
       :type integer
       :primary-key t
       :auto-increment t
       :initform nil
       :initarg :id))
  (:metaclass sql-table))

(defun lisp-type->sql-type (type)
  (case type
    ((t) "BLOB")
    (otherwise (symbol-name type))))

(defgeneric slot-definition-sql (slotd))
(defmethod slot-definition-sql ((slotd sql-table-effective-slot-definition))
  (concatenate 'string (symbol-name (sql-name slotd)) " "
               (lisp-type->sql-type (mop:slot-definition-type slotd))
               (when (autoincrement slotd) " AUTO_INCREMENT")
               (when (primary-key slotd) " PRIMARY KEY NOT NULL")
               (a:when-let (ref (references slotd))
                   (format nil " FOREIGN KEY REFERENCES ~a(~a)"
                           (lisp-identifier->sql-identifier (first ref))
                           (lisp-identifier->sql-identifier (second ref))))))

(defgeneric create-table-sql (class))
(defmethod create-table-sql ((class sql-table))
  (format nil "CREATE TABLE ~a (~{~a~^,~});" (sql-name class)
          (mapcar #'slot-definition-sql (mop:class-slots class))))

(defgeneric save-instance-of (class instance database)
(defmethod save-instance-of ((class sql-table) instance database)

  ;; TODO create a utility function to get the primary key of a class
  (let ((primary-key-slot (mop:slot-definition-name
                           (first
                            (remove-if-not #'primary-key (mop:class-slots class))))))
    (if primary-key-slot
        t

        ;; else
        (multiple-value-bind (sql-names slot-names)
            (loop :for slot :in (mop:class-slots class)
                  :for name = (mop:slot-definition-name slot)
                  :when (slot-boundp instance name)
                    :collect (sql-name slot) :into sql-names
                    :and :collect name :into slot-names
                  :finally (return (values sql-names slot-names)))
          (multiple-value-bind (sql data)
              (sxql:yield (sxql:insert-into (sql-name class)
                            sql-names
                            (mapcar (a:curry #'slot-value instance) slot-names)))
            (dbi:do-sql database sql data))))))

(defun save-instance (instance database)
  ()
  )

;; (defgeneric insert-into-table-sql (class))
;; (defmethod insert-into-table-sql ((class sql-table))
;;   (format nil "INSERT INTO ~a(~{~a~^,~}) VALUES(~{~a~^,~});"))


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
