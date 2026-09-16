(defpackage #:open-orders.sql-table
  (:use #:cl)
  (:export

   #:sql-table

   #:select
   #:insert
   #:drop
   #:create
   #:update

   #:exec
   #:statement
   #:make-statement
   #:statement-p
   #:copy-statement
   #:statement-sql
   #:statement-params
   #:statement-fetch
   #:statement-fetch-results-parse-function
   #:sql-table-direct-slot-definition
   #:slot-persistent-p
   #:slot-autoincrement-p
   #:slot-not-null-p
   #:slot-references
   #:slot-primary-key-p
   #:sql-value->lisp-value
   #:lisp-value->sql-value
   #:lisp-type->sql-type
   #:lisp-name->sql-name
   #:references-form-p
   #:select-all
   #:*database-handle*))
(in-package #:open-orders.sql-table)

;;;; ==== OPTIONAL DEPENDENCIES ====
(eval-when (:compile-toplevel :load-toplevel)
  (when (find-package '#:dbi)
    (pushnew :dbi *features*))
  (when (find-package '#:marshal)
    (pushnew :marshal *features*)))


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
  (unless (or (null value)
              (typep value type))
    (break "Found ~a of type ~a, expected type ~a" value (type-of value) type))
  (cond
    ((subtypep type 'real) value)
    ((eq type 'string) value)
    ((eq type 'symbol) (concatenate 'string
                                    (package-name (symbol-package value))
                                    "::"
                                    (symbol-name value)))
    ((eq type 'boolean) (if value 1 0))
    ((subtypep type 'real) value)
    (t #+marshal(format nil "~S" (marshal:marshal value))
       #-marshal
       (error "Cannot serialize this type of object without cl-marshal"))))

(defun sql-value->lisp-value (type value)
  (cond
    ((null value) nil)
    ((subtypep type 'integer) value)
    ((eq type 'string) (format nil "~a" value))
    ((eq type 'symbol) (let ((*read-eval* nil)) (read-from-string value)))
    ((eq type 'boolean) (if (= value 1) t nil))
    ((subtypep type 'real) value)

    (t
     #+marshal (marshal:unmarshal
                (let ((*read-eval* nil)) (read-from-string value)))
     #-marshal
     (error "Cannot deserialize this object without cl-marshal"))))


;;;; ==== DEFINITIONS ====
(defstruct column
  (symbol nil :type symbol)
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
  (let ((name (closer-mop:slot-definition-name slotd))
        (type (closer-mop:slot-definition-type slotd)))
    (make-column
     :symbol name
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
  (closer-mop:ensure-finalized class)
  (let ((name (class-name class))
         (slots (closer-mop:class-slots class)))
    (flet ((persistent-slots ()
             (let* ((names (mapcar #'closer-mop:slot-definition-name slots))
                    (persistent (remove-if-not
                                 (lambda (slot-name)
                                   (slot-persistent-p name slot-name))
                                 names)))
               (remove-if-not
                (lambda (s) (member (closer-mop:slot-definition-name s)
                                    persistent))
                slots))))
      (make-table :name (lisp-name->sql-name name)
                  :columns (mapcar (lambda (slot)
                                     (slotd->column name slot))
                                   (persistent-slots))))))

(declaim (ftype (function (table symbol) (or column null))
                table-lookup-column))
(defun table-lookup-column (table slot-name)
  "Give a sql-table class and a slot name,
   return the column definition for that slot"
  (find slot-name (table-columns table) :key #'column-symbol))

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
                      (let ((ref (column-references c)))
                        (when ref
                          (format nil " REFERENCES ~a(~a)"
                                  (first ref) (second ref))))))
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
    (closer-mop:ensure-finalized class)
    class))

;;;; ==== METACLASS ====
(defclass sql-table (standard-class) ())
(defmethod closer-mop:validate-superclass ((class sql-table)
                                    (superclass standard-class))
  t)
(defclass sql-table-direct-slot-definition
    (closer-mop:standard-direct-slot-definition)
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
(defmethod closer-mop:direct-slot-definition-class
    ((class sql-table) &rest initargs)
  (declare (ignore initargs))
  (find-class 'sql-table-direct-slot-definition))

(defmethod closer-mop:compute-effective-slot-definition
    ((class sql-table) name direct-slots)
  (unless (every (lambda (slotd)
                   (eq (class-of slotd)
                       (find-class 'sql-table-direct-slot-definition)))
                 direct-slots)
    (error "Invalid direct slot class for ~a" direct-slots))

  (let ((eslot (call-next-method))
        (slot-name (some #'closer-mop:slot-definition-name direct-slots))
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


(defstruct statement
  (sql "" :type string)
  (values-only-p t :type boolean)
  params
  (fetch t :type (member t nil :all) )
  fetch-results-parse-function)



(defun instance->sql-names (instance)
  (loop
    :with class = (class-of instance)
    :with table = (class->table class)
    :for col :in (table-columns table)
    :when (slot-boundp instance (column-lisp-name col))
      :collect (column-name col)))

(defun instance->sql-values (instance)
  (loop
    :with class = (class-of instance)
    :with table = (class->table class)
    :for col :in (table-columns table)
    :when (slot-boundp instance (column-lisp-name col))
      :collect (lisp-value->sql-value
                (column-lisp-type col)
                (slot-value instance (column-lisp-name col)))))

(defun instance->primary-key-value (instance)
  (slot-value instance
              (column-lisp-name
               (table.primary-key.column (class->table (class-of instance))))))

;;;; ==== PUBLIC API ====
(defvar *database-handle* nil)
(defgeneric exec (database-handle statement))

(defun make-create-statement (classname &key if-not-exists)
  (make-statement
   :sql (table.sql.create-table (class->table
                                 (find-finalized-class classname))
                                :if-not-exists if-not-exists)))
(defun create (classname &key if-not-exists (database-handle *database-handle*))
  (exec database-handle (make-create-statement
                         classname :if-not-exists if-not-exists)))

(defun make-drop-statement (classname &key if-exists)
  (make-statement
   :sql (table.sql.drop-table (class->table
                               (find-finalized-class classname))
                              :if-exists if-exists)))
(defun drop (classname &key if-exists (database-handle *database-handle*))
  (exec database-handle (make-drop-statement
                         classname :if-exists if-exists)))

(defun make-insert-statement (instance)
  (let* ((class (class-of instance))
         (table (class->table class)))
    (make-statement
     :sql (table.sql.insert-into table :column-names (instance->sql-names instance))
     :params (instance->sql-values instance)
     :fetch t
     :fetch-results-parse-function (lambda (result) (first result)))))
(defun insert (classname &key (database-handle *database-handle*))
  (exec database-handle (make-insert-statement classname)))

(defun parse-select-statement-results (values names types classname)
  "Parses a list of sql values the data contained in statement"
  (let ((instance (make-instance classname)))
    (assert (= (length values)
               (length types)
               (length names)))
    (loop :for val :in values
          :for name :in names
          :for type :in types
          :do (setf (slot-value instance name)
                    (sql-value->lisp-value type val)))
    instance))

(defun make-select-statement (classname where-column where-value)
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
               (mapcar #'closer-mop:slot-definition-name
                       (closer-mop:class-slots (find-class classname)))
               (mapcar #'closer-mop:slot-definition-type
                       (closer-mop:class-slots (find-class classname)))
               classname))))))
(defun select (classname where-column where-value
               &key (database-handle *database-handle*))
  (exec database-handle (make-select-statement classname where-column
                                               where-value)))


(defun make-select-all-statement (classname)
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
(defun select-all (classname &key (database-handle *database-handle*))
  (exec database-handle (make-select-all-statement classname)))

(defun make-update-statement (instance)
  (let ((table (class->table (class-of instance))))
    (make-statement
     :sql (table.sql.update table
                            :column-names (instance->sql-names instance))
     :params (append (instance->sql-values instance)
                     (list
                      (instance->primary-key-value instance))))))
(defun update (instance &key (database-handle *database-handle*))
  (exec database-handle (make-update-statement instance)))

;;;; OPTIONAL DBI INTEGRATION
#+dbi
(defmethod exec ((database-handle dbi:dbi-connection) statement)
  (let ((query (dbi:prepare database-handle (statement-sql statement))))
    (dbi:execute query (statement-params statement))
    (ecase (statement-fetch statement)
      ((:all)
       (funcall
        (statement-fetch-results-parse-function statement)
        (dbi:fetch-all query :format (if (statement-values-only-p statement) :values
                                         :alist))))
      ((t)
       (unless (zerop (dbi:query-row-count query))
         (funcall (statement-fetch-results-parse-function statement)
                  (dbi:fetch query :format (if (statement-values-only-p
                                                statement)
                                               :values
                                               :alist)))))
      ((nil) query))))




;; Query Statement
(defstruct ref
   "'slot' -> The symbol representing this slot in your sql-table class definition.
    'as'   ->  The name that the resulting plist will identify this slot as.
    'foreign-refs' -> If the slot is a foreign key, you can specify
        this option to automatically select slots from the foreign object as well"
  (slot (error "Required slot") :type symbol)
  (as nil :type (or string symbol)) ;; TODO actually support this "as"
  (foreign-refs nil :type list))

(defun ref.sql.columns-and-joins (table ref-list)
  (let ((columns nil)
        (joins nil))

    ;; iterate over all foreign fields
    (dolist (ref ref-list)
      (let* ((column
               (table-lookup-column
                table (ref-slot ref)))
             (_ (assert column))
             (primary-column-sql
               (format nil "~a.~a AS ~a_~a"
                       (table-name table) (column-name column)
                       (table-name table) (column-name column)))
             (references (column-references column))
             (foreign-refs (ref-foreign-refs
                            ref)))
        (declare (ignore _))

        (when foreign-refs
          (unless references
            (error "Cannot do a foreign field query on a column that does
                not reference another table: ~a" foreign-refs))
          
          (let* ((foreign-table-name (first references))
                 (foreign-column-name (second references))
                 (foreign-table (class->table
                                 (find-finalized-class foreign-table-name)))
                 (foreign-column
                   (table-lookup-column foreign-table foreign-column-name)))
            (assert foreign-column)
            (multiple-value-bind (foreign-columns foreign-joins)
                (ref.sql.columns-and-joins
                 foreign-table foreign-refs)
              (setf columns (append columns foreign-columns))
              (setf joins (append joins foreign-joins)))
            
            (push (format nil "LEFT JOIN ~a ON ~a.~a = ~a.~a"
                          (table-name foreign-table)
                          (table-name foreign-table)
                          (column-name foreign-column)
                          (table-name table)
                          (column-name column))
                  joins)))

        (push primary-column-sql columns)))
    (values columns joins)))


(defun make-query-statement (classname ref-list)
  (let* ((class (find-finalized-class classname))
         (table (class->table class)))
    (multiple-value-bind (columns joins)
        (ref.sql.columns-and-joins table ref-list)
      (make-statement
       :sql (concatenate
             'string
             "SELECT"
             (format nil "~{ ~a~^,~}" columns)
             " FROM " (table-name table)
             (format nil "~{ ~a~}" joins)
             ";")
       :fetch :all
       :values-only-p nil
       :fetch-results-parse-function
       (lambda (values)
         values)
       ))))
(defun query (classname ref-list &key (database-handle *database-handle*))
  (exec database-handle (make-query-statement classname ref-list)))


;; (defun test ()
;;   (query
;;    'open-orders.tables:open-order
;;    (list (make-ref
;;           :slot 'open-orders.tables:purchase-order)
;;          (make-ref :slot 'open-orders.tables:line-item)
;;          (make-ref
;;           :slot 'open-orders.tables:part
;;           :foreign-refs
;;           (list (make-ref
;;                  :slot 'open-orders.tables:part-number))))))


