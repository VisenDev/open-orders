(defpackage #:open-orders.sql-table
  (:use #:cl)
  (:import-from #:trivial-types
                #:association-list)
  (:local-nicknames (#:a #:alexandria)
                    (#:mop #:closer-mop)))
(in-package #:open-orders.sql-table)

(declaim (optimize (debug 3)))

;;;; ==== TYPES ====
(declaim (ftype (function (t) boolean) references-form-p))
(defun references-form-p (form)
  (or (null form)
      (and (listp form)
           (= (length form) 2)
           (symbolp (first form))
           (symbolp (second form)))))
(deftype references-form () `(satisfies references-form-p))


;;;; ==== CONVERSIONS ====
(declaim (ftype (function (symbol) string) lisp-name->sql-name))
(defun lisp-name->sql-name (lisp-symbol)
  "Converts a lisp symbol to a valid sql identifier string"
  (map 'string (lambda (ch)
                 (if (alphanumericp ch) (char-upcase ch) #\_))
       (symbol-name lisp-symbol)))

(defun lisp-type->sql-type (type)
  (cond
    ((subtypep type 'integer) "INTEGER")
    ((eq type 'string) "STRING")
    ((eq type 'symbol) "STRING")
    ((eq type 'boolean) "BOOLEAN")
    ((subtypep type 'real) "REAL")
    (t "BLOB")))

(defun lisp-value->sql-value (type value)
  (assert (typep value type))
  (cond
    ((subtypep type 'integer) value)
    ((eq type 'string) value)
    ((eq type 'symbol) (concatenate 'string
                                    (package-name (symbol-package value))
                                    "::"
                                    (symbol-name value)))
    ((eq type 'boolean) (if value 1 0))
    ((subtypep type 'real) value)
    (t (format nil "~S" (marshal:marshal value)))))

(defun sql-value->lisp-value (type value)
  (cond
    ((subtypep type 'integer) value)
    ((eq type 'string) value)
    ((eq type 'symbol) (let ((*read-eval* nil)) (read-from-string value)))
    ((eq type 'boolean) (if (= value 1) t nil))
    ((subtypep type 'real) value)
    (t (marshal:unmarshal (let ((*read-eval* nil)) (read-from-string value))))))


;;;; ==== DEFINITIONS ====
(defstruct column
  (name "" :type string)
  (type "" :type string)
  (primary-key-p nil :type boolean)
  (references '() :type references-form)
  (autoincrement-p nil :type boolean)
  (not-null-p nil :type boolean))


(defgeneric slot-primary-key-p (classname slotname))
(defmethod  slot-primary-key-p (classname slotname) nil)

(defgeneric slot-references (classname slotname))
(defmethod slot-references (classname slotname) nil)

(defgeneric slot-not-null-p (classname slotname))
(defmethod slot-not-null-p (classname slotname) nil)

(defgeneric slot-autoincrement-p (classname slotname))
(defmethod slot-autoincrement-p (classname slotname) nil)

(defgeneric slot-persistent-p (classname slotname))
(defmethod slot-persistent-p (classname slotname) t)

(defgeneric slotd->column (classname slotd))
(defmethod  slotd->column (classname slotd)
  (let ((name (mop:slot-definition-name slotd)))
    (make-column
     :name (lisp-name->sql-name name)
     :type (lisp-type->sql-type (mop:slot-definition-type slotd))
     :primary-key-p (slot-primary-key-p classname name)
     :references (slot-references classname name)
     :autoincrement-p (slot-autoincrement-p classname name)
     :not-null-p (slot-not-null-p classname name))))

(defstruct table
  (name "" :type string)
  (columns nil :type list))

(defmethod class->table ((class standard-class))
  (let* ((name (class-name class))
         (slots (mop:class-slots class)))
    (flet ((persistent-slots ()
             (let* ((names (mapcar #'mop:slot-definition-name slots))
                    (persistent (remove-if-not
                                 (a:curry #'slot-persistent-p name)
                                 names)))
               (remove-if-not
                (lambda (s) (member (mop:slot-definition-name s) persistent))
                slots))))
      (make-table :name (lisp-name->sql-name name)
                  :columns (mapcar (lambda (slot)
                                     (slotd->column name slot))
                                   (persistent-slots))))))

(defstruct statement sql params)

(defun table->create-table-sql (table &key if-not-exists)
  (concatenate
   'string "CREATE TABLE "
   (when if-not-exists "IF NOT EXISTS ")
   (table-name table)
   (format nil " (~{~a~^, ~});"
           (mapcar (lambda (c)
                     (concatenate
                      'string
                      (column-name c) " "
                      (column-type c)
                      (when (column-primary-key-p c) " PRIMARY KEY")
                      (when (column-autoincrement-p c) " AUTO_INCREMENT")
                      (when (column-not-null-p c) " NOT NULL")
                      (a:when-let (ref (column-references c))
                        (format nil " REFERENCES ~a(~a)" (first ref) (second ref)))))
                   (table-columns table)))))

(defun table->drop-table-sql (table &key if-exists)
  (format nil "DROP TABLE ~:[~;IF EXISTS ~]~a;" if-exists (table-name table)))

(defmethod insert-sql (class))

;;;; testing

(defclass person ()
  (id first-name last-name age))

(defmethod slot-primary-key-p ((classname (eql 'person)) (slotname (eql 'id))) t)

(defun test ()
  (mop:ensure-finalized (find-class 'person))
  (let ((tbl
          (class->table (find-class 'person))))

    (table->drop-table-sql tbl :if-exists t))
  )


    
