(defpackage #:open-orders.mop
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:mop #:closer-mop))
  (:export #:sql-table
           #:database-initialize
           #:database-lookup
           #:database-create-table
           #:database-drop-table
           #:database-update
           #:database-insert
           #:database-alter-table-schema-if-needed))
(in-package #:open-orders.mop)

(declaim (optimize (debug 3)))

(defun slot-value? (object slot &optional default)
  (if (slot-boundp object slot)
      (slot-value object slot)
      default))

(defun lisp-identifier->sql-identifier (ident)
  (map 'string (lambda (ch)
                 (if (alphanumericp ch) ch #\_))
       (symbol-name ident)))

(defclass sql-table (standard-class)
  ((sql-name :accessor sql-name)
   (schema-metadata :accessor schema-metadata :initform nil)
   (primary-key-slot :accessor primary-key-slot :initform nil)
   (slots-to-insert :accessor slots-to-insert)))

(defmethod derive-slots-to-insert ((class sql-table))
  (remove-if #'autoincrement (mop:class-slots class)))

(defgeneric find-primary-key-slot (class))
(defgeneric derive-schema-metadata (class))

(defmethod mop:finalize-inheritance :after ((class sql-table))
  (setf (sql-name class)
        (lisp-identifier->sql-identifier (class-name class)))
  (setf (schema-metadata class)
        (derive-schema-metadata class))
  (setf (primary-key-slot class)
        (find-primary-key-slot class))
  (setf (slots-to-insert class)
        (derive-slots-to-insert class)))

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

(defmethod find-primary-key-slot ((class sql-table))
  (loop :for slot :in (mop:class-slots class)
        :when (primary-key slot)
          :do (return-from find-primary-key-slot slot)))

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

(defgeneric slot-fingerprint (slotd))
(defmethod slot-fingerprint ((slotd sql-table-effective-slot-definition))
  (list (sql-name slotd)
        (lisp-type->sql-type (mop:slot-definition-type slotd))
        (primary-key slotd)
        (references slotd)
        (autoincrement slotd)))

(defun slot-fingerprint-name (fingerprint) (first fingerprint))
(defun slot-fingerprint-type (fingerprint) (second fingerprint))
(defun slot-fingerprint-primary-key-p (fingerprint) (third fingerprint))
(defun slot-fingerprint-references (fingerprint) (fourth fingerprint))
(defun slot-fingerprint-autoincrement-p (fingerprint) (fifth fingerprint))

(defgeneric class-fingerprint (class))
(defmethod class-fingerprint ((class sql-table))
  (mop:ensure-finalized class)
  (format
   nil "~a"
   (mapcar #'slot-fingerprint (mop:class-slots class))))

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
    ((t) "STRING")
    (otherwise (symbol-name type))))

(defgeneric slot-definition-sql (slotd))
(defmethod slot-definition-sql ((slotd sql-table-effective-slot-definition))
  (concatenate 'string (sql-name slotd) " "
               (lisp-type->sql-type (mop:slot-definition-type slotd))
               (when (autoincrement slotd) " AUTO_INCREMENT")
               (when (primary-key slotd) " PRIMARY KEY NOT NULL")
               (a:when-let (ref (references slotd))
                   (format nil " FOREIGN KEY REFERENCES ~a(~a)"
                           (lisp-identifier->sql-identifier (first ref))
                           (lisp-identifier->sql-identifier (second ref))))))

(defclass schema-metadata ()
  ((name :type string :accessor name :primary-key t :initarg :name)
   (hash :type integer :accessor hash :initarg :hash)
   (fingerprint :type string :accessor fingerprint :initarg :fingerprint))
  (:metaclass sql-table))

(defmethod derive-schema-metadata ((class sql-table))
  (let ((fingerprint (class-fingerprint class)))
    (make-instance 'schema-metadata
                   :name (sql-name class)
                   :fingerprint fingerprint
                   :hash (sxhash fingerprint))))

(defgeneric class-slots-sql (class))
(defmethod class-slots-sql ((class sql-table))
  "Returns a list of the slot definition sql for a class"
  (mapcar #'slot-definition-sql (mop:class-slots class)))

(defgeneric create-table-sql (class &key if-not-exists))
(defmethod create-table-sql ((class sql-table) &key if-not-exists)
  (mop:ensure-finalized class)
  (format nil "CREATE TABLE ~a~a (~{~a~^, ~});"
          (if if-not-exists "IF NOT EXISTS " "")
          (sql-name class)
          (class-slots-sql class)))

(defgeneric drop-table-sql (class &key if-exists))
(defmethod drop-table-sql ((class sql-table) &key if-exists)
  (mop:ensure-finalized class)
  (format nil "DROP TABLE ~a~a;" (if if-exists "IF EXISTS " "") (sql-name class)))

(defun database-create-table (database classname &key if-not-exists)
  (dbi:do-sql database
    (create-table-sql (find-class classname)
                      :if-not-exists if-not-exists))
  (if (database-lookup database 'schema-metadata (sql-name (find-class classname)))
      (database-update database (schema-metadata (find-class classname)))
      (database-insert database (schema-metadata (find-class classname)))))


(defun database-drop-table (database classname &key if-exists)
  (dbi:do-sql database (drop-table-sql (find-class classname)
                                       :if-exists if-exists))
  ;; TODO add a call which removes the entry here
  )


(defmethod insert-sql ((class sql-table))
  (let* ((slots (slots-to-insert class))
         (sql-names (mapcar #'sql-name slots)))
    (format nil "INSERT INTO ~a(~{~a~^, ~}) VALUES (~{~a~^, ~});"
            (sql-name class)
            sql-names
            (mapcar (constantly #\?) sql-names))))

;; (defmethod marshal:class-persistent-slots ((class standard-object))
;;   (mop:ensure-finalized (class-of class))
;;   (mapcar #'mop:slot-definition-name (mop:class-slots (class-of class))))

(defun lisp-object->sql-object (type value)
  (case type
    ((integer fixnum string boolean) value)
    ((symbol) (symbol-name value))
    (otherwise (format nil "~s" (marshal:marshal value)))))

(defun sql-object->lisp-object (type value)
  (case type
    ((integer fixnum string boolean) value)
    ((symbol) (intern value))
    (otherwise
     (marshal:unmarshal
      (let ((*read-eval* nil))
        (read-from-string value))))))

(defmethod collect-sql-objects ((class sql-table) instance slots)
  (loop :for slot :in slots
        :collect (lisp-object->sql-object
                  (mop:slot-definition-type slot)
                  (slot-value? instance (mop:slot-definition-name slot)))))

(defmethod collect-sql-objects-for-insert ((class sql-table) instance)
  (collect-sql-objects class instance (slots-to-insert class)))

(defmethod collect-sql-objects-for-update ((class sql-table) instance)
  (collect-sql-objects class instance (mop:class-slots class)))

(defun database-insert (database instance)
  (dbi:do-sql database
    (insert-sql (class-of instance))
    (collect-sql-objects-for-insert (class-of instance) instance)))

(defmethod lookup-sql ((class sql-table) column-sql-name)
  (format nil "SELECT ~{~a~^, ~} FROM ~a WHERE ~a = ?;"
          (mapcar #'sql-name (mop:class-slots class))
          (sql-name class)
          column-sql-name))

(defun database-lookup (database classname key &key column)
  (let* ((class (find-class classname))
         (columnname (if column
                         (lisp-identifier->sql-identifier column)
                         (sql-name (primary-key-slot class))))
         (query (dbi:prepare database (lookup-sql class columnname)))
         (results (dbi:fetch (dbi:execute query (list key)) :format :values))
         (result (make-instance classname)))
    (when (null results)
      (return-from database-lookup nil))
    (loop :for slot :in (mop:class-slots class)
          :for name = (mop:slot-definition-name slot)
          :for type = (mop:slot-definition-type slot)
          :for value :in results
          :do (setf (slot-value result name)
                    (sql-object->lisp-object type value))
          :finally (return result))))

;; (defun database-lookup-all (database classname key &key column)
;;   (multiple-value-bind (first-result query)
;;       (database-lookup database classname key :column column)
;;     (cons first-result
;;           (loop :with n = (dbi:query-row-count query)
;;                 :repeat n
;;                 :collect (dbi:fetch query)))))

(defmethod update-sql ((class sql-table))
  (format nil "UPDATE ~a SET ~{~a = ?~^, ~} WHERE ~a = ?;"
          (sql-name class)
          (mapcar #'sql-name (mop:class-slots class))
          (sql-name (primary-key-slot class))))

(defun database-update (database instance)
  (let ((class (class-of instance)))
    (assert (primary-key-slot class)) ;; Class should have a primary key

    
    
    (let* ((sql (update-sql class))
           (query (dbi:prepare database sql)))
      (dbi:execute query
                   (append
                    (collect-sql-objects-for-update class instance)
                    (list (lisp-object->sql-object
                           (mop:slot-definition-type
                            (primary-key-slot class))
                           (slot-value instance (mop:slot-definition-name
                                                 (primary-key-slot class))))))))))

(defun migrate-table-using-fingerprints (database tablename &key old new)
  (when (equalp old new)
    (return-from migrate-table-using-fingerprints))

  (let ((deleted-cols (set-difference old new :test #'equalp) )
        (added-cols (set-difference new old :test #'equalp) ))
    ;; (loop :for col :in deleted-cols)
    (declare (ignorable deleted-cols))
    (loop :for col :in added-cols
          :do
             (when (slot-fingerprint-autoincrement-p col)
               (error "Cannot add new autoincrement row"))
             (when (slot-fingerprint-primary-key-p col)
               (error "Cannot add new primary key row"))
             (when (slot-fingerprint-references col)
               (error "Unable to satisfy foreign key constraint in new row"))
             (dbi:do-sql database
               (format nil "ALTER TABLE ~a ADD ~a ~a;"
                       tablename
                       (slot-fingerprint-name col)
                       (slot-fingerprint-type col))))))

(defun database-alter-table-schema-if-needed (database classname)
  (let ((*read-eval* nil)
        (class (find-class classname)))
    (a:when-let (metadata (database-lookup database 'schema-metadata
                                                  (sql-name class)))
      (let ((fingerprint (read-from-string (fingerprint metadata))))
        (unless (equalp fingerprint (fingerprint (schema-metadata class)))
          (migrate-table-using-fingerprints database (sql-name class)
                                            :old fingerprint
                                            :new (read-from-string (fingerprint (schema-metadata class))))
          (database-update database (schema-metadata class)))))))

(defun database-initialize (database)
  (database-create-table database 'schema-metadata :if-not-exists t))


;;; TESTING
#+NIL
(defclass person (standard-sql-table)
  ((first-name :accessor first-name :initarg :first-name)
   (last-name :accessor last-name :initarg :last-name)
   (email :accessor email)
   (phone :accessor phone)
   (notes :accessor notes :type string))
  (:metaclass sql-table))

#+NIL
(defclass company (standard-sql-table)
  ((primary-contact :accessor primary-contact))
  (:metaclass sql-table))

