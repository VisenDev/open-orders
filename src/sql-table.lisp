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
    ((null value) nil)
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
  (lisp-name nil :type symbol)
  (lisp-type nil :type symbol)
  (primary-key-p nil :type boolean)
  (autoincrement-p nil :type boolean)
  (references '() :type references-form)
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
  (let ((name (mop:slot-definition-name slotd))
        (type (mop:slot-definition-type slotd)))
    (make-column
     :name (lisp-name->sql-name name)
     :type (lisp-type->sql-type type)
     :lisp-name name
     :lisp-type type
     :primary-key-p (slot-primary-key-p classname name)
     :references (slot-references classname name)
     :autoincrement-p (slot-autoincrement-p classname name)
     :not-null-p (slot-not-null-p classname name))))

(defstruct table
  (name "" :type string)
  (columns nil :type list))

(defmethod class->table ((class standard-class))
  (mop:ensure-finalized class)
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



(defun table.sql.create-table (table &key if-not-exists)
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
                      (when (column-autoincrement-p c) " AUTOINCREMENT")
                      (when (column-not-null-p c) " NOT NULL")
                      (a:when-let (ref (column-references c))
                        (format nil " REFERENCES ~a(~a)" (first ref) (second ref)))))
                   (table-columns table)))))

(defun table.column-names (table)
  (mapcar #'column-name (table-columns table)))

(defun table.column-types (table)
  (mapcar #'column-type (table-columns table)))

(defun table.column-lisp-types (table)
  (mapcar #'column-lisp-type (table-columns table)))

(defun table.column-lisp-names (table)
  (mapcar #'column-lisp-name (table-columns table)))

(defun table.primary-key.column (table)
  (find t (table-columns table) :key #'column-primary-key-p))

(defun table.primary-key.name (table)
  (column-name (table.primary-key.column table)))

(defun table.sql.drop-table (table &key if-exists)
  (format nil "DROP TABLE ~:[~;IF EXISTS ~]~a;" if-exists (table-name table)))

(defun table.sql.insert-into
    (table &key
             (column-names (table.column-names table))
             (returning (table.primary-key.name table)))
  (format
   nil "INSERT INTO ~a(~{~a~^,~}) VALUES (~{~a~^,~})~@[ RETURNING ~a~];"
          (table-name table)
          column-names
          (mapcar (constantly #\?) column-names)
          returning))

(defun table.sql.update
    (table &key
             (column-names (table.column-names table))
             (where (table.primary-key.name table)))
  (format nil "UPDATE ~a SET ~{~a = ?~^, ~} WHERE ~a = ?;"
          (table-name table)
          column-names
          where))

(defun table.sql.select
    (table &key
             (column-names (table.column-names table))
             (where (table.primary-key.name table)))
  (format nil "SELECT ~{~a~^, ~} FROM ~a~@[ WHERE ~a = ?~];"
          column-names (table-name table) where))

(defun find-finalized-class (classname)
  "Finds a class and ensures it is finalized as well"
  (let ((class (find-class classname)))
    (mop:ensure-finalized class)
    class))

;;;; ==== METACLASS ====
(defclass sql-table (standard-class) ())
(defmethod mop:validate-superclass ((class sql-table)
                                    (superclass standard-class))
  t)
(defclass sql-table-direct-slot-definition
    (mop:standard-direct-slot-definition)
  ((primary-key-p :initarg :primary-key
                  :initform nil
                  :accessor primary-key-p)
   (persistent-p :initarg :persistent
                 :initform t
                 :accessor persistent-p)
   (autoincrement-p :initarg :autoincrement
                    :initform nil
                    :accessor autoincrement-p)
   (references :initarg :references
               :initform nil
               :accessor references)
   (not-null-p :initarg :not-null
               :initform nil
               :accessor not-null-p)))
(defmethod mop:direct-slot-definition-class
    ((class sql-table) &rest initargs)
  (declare (ignore initargs))
  (find-class 'sql-table-direct-slot-definition))
(defmethod mop:compute-effective-slot-definition
    ((class sql-table) name direct-slots)
  (let ((eslot (call-next-method))
        (slot-name (some #'mop:slot-definition-name direct-slots))
        (class-name (class-name class))
        (primary-key-p (some #'primary-key-p direct-slots))
        (persistent-p (some #'persistent-p direct-slots))
        (autoincrement-p (some #'autoincrement-p direct-slots))
        (references (some #'references direct-slots))
        (not-null-p (some #'not-null-p direct-slots)))

    ;; define methods using direct slot metadata
    (defmethod slot-primary-key-p
        ((classname (eql class-name))
         (slotname (eql slot-name)))
      primary-key-p)
    (defmethod slot-persistent-p
        ((classname (eql class-name))
         (slotname (eql slot-name)))
      persistent-p)
    (defmethod slot-autoincrement-p
        ((classname (eql class-name))
         (slotname (eql slot-name)))
      autoincrement-p)
    (defmethod slot-references
        ((classname (eql class-name))
         (slotname (eql slot-name)))
      references)
    (defmethod slot-not-null-p
        ((classname (eql class-name))
         (slotname (eql slot-name)))
      not-null-p)
    
    eslot))

;;;; ==== PUBLIC API ====
(defstruct statement
  sql
  params

  ;; extra data that can be used for parsing the
  ;; results of select statements automatically
  ;; selected-slot-names
  ;; selected-slot-types
  ;; selected-classname
  fetch
  fetch-results-parse-function 
  )

(defun create-table (classname)
  (make-statement
   :sql (table.sql.create-table (class->table 
                                 (find-finalized-class classname)))))

(defun drop-table (classname)
  (make-statement
   :sql (table.sql.drop-table (class->table 
                                 (find-finalized-class classname)))))

(defun insert-into (instance)
  (let*
      ((class (class-of instance))
       (slots (mop:class-slots class))
       (bound-slots
         (remove-if-not (a:curry #'slot-boundp instance) slots
                        :key #'mop:slot-definition-name))
       (table (class->table class)))
    (when (null bound-slots)
      (return-from insert-into (make-statement :sql "")))
    (make-statement
     :sql (table.sql.insert-into table
                                 :column-names
                                 (mapcar (a:compose #'lisp-name->sql-name
                                                    #'mop:slot-definition-name)
                                         bound-slots))
     :params (mapcar (lambda (slotd)
                       (lisp-value->sql-value
                        (mop:slot-definition-type slotd)
                        (slot-value instance (mop:slot-definition-name slotd))))
                     bound-slots))))

(defun parse-select-statement-results (values names types classname)
  "Parses a list of sql values the data contained in statement"
  (let* ((instance (make-instance classname)))
    (assert (= (length values)
               (length types)
               (length names)))
    (loop :for val :in values
          :for name :in names
          :for type :in types
          :do (setf (slot-value instance name)
                    (sql-value->lisp-value type val)))
    instance))

(defun select (classname where-column where-value)
  (let ((table (class->table (find-finalized-class classname))))
    (make-statement
     :sql (table.sql.select table :column-names (table.column-names table)
                                  :where (lisp-name->sql-name where-column))
     :params (list where-value)
     :fetch t
     :fetch-results-parse-function
     (lambda (value)
       (print (parse-select-statement-results
               value
               (mapcar #'mop:slot-definition-name
                       (mop:class-slots (find-class classname)))
               (mapcar #'mop:slot-definition-type
                       (mop:class-slots (find-class classname)))
               classname))))))

(defun select-all (classname)
  (let ((table (class->table (find-finalized-class classname))))
    (make-statement
     :sql (table.sql.select table :column-names (table.column-names table)
                                  :where nil)
     :params nil
     :fetch :all
     :fetch-results-parse-function
     (lambda (values)
       (mapcar (lambda (value)
                 (parse-select-statement-results
                  value
                  (table.column-lisp-names table)
                  (table.column-lisp-types table)
                  classname))
               values)))))



;;;; DBI INTEGRATION
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (find-package '#:dbi)
    (pushnew :dbi *features*)))

(defgeneric do-statement (database statement))

#+dbi
(defmethod do-statement
    ((database dbi:dbi-connection)
     (statement statement))
  (let ((query (dbi:prepare database (statement-sql statement))))
    (dbi:execute query (statement-params statement))
    (ecase (statement-fetch statement)
      ((:all)
       (funcall
        (statement-fetch-results-parse-function statement)
        (dbi:fetch-all query :format :values)))
      ((t)
       (unless (zerop (dbi:query-row-count query))
         (funcall (statement-fetch-results-parse-function statement)
                  (dbi:fetch query :format :values))))
      ((nil) query))))

;;;; testing

(defclass person ()
  ((id :type integer
       :primary-key t
       :autoincrement t
       :not-null t)
   (first-name :initarg :first)
   last-name age)
  (:metaclass sql-table))

(defmethod slot-primary-key-p ((classname (eql 'person)) (slotname (eql 'id))) t)

(defun test ()
  (select 'person 'id 0))
