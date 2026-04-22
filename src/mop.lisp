(defpackage #:open-orders.mop
  (:use #:cl)
  (:local-nicknames (#:db #:open-orders.database)
                    (#:a #:alexandria)
                    (#:mop #:closer-mop)))
(in-package #:open-orders.mop)

(defun slot-value? (object slot &optional default)
  (if (slot-boundp object slot)
      (slot-value object slot)
      default))

(defun lisp-identifier->sql-identifier (ident)
  (map 'string (lambda (ch)
                 (if (alphanumericp ch) ch #\_))
       (symbol-name ident)))

(defmethod derive-slots-to-insert ((class sql-table))
  (remove-if #'autoincrement (mop:class-slots class)))

(defclass sql-table (standard-class)
  ((sql-name :accessor sql-name)
   (schema-metadata :accessor schema-metadata :initform nil)
   (primary-key-slot :accessor primary-key-slot :initform nil)
   (slots-to-insert :accessor slots-to-insert)))

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

;; (defun database-ensure-metadata-table-exists (database)
;;   (dbi:do-sql database
;;     "CREATE TABLE IF NOT EXISTS SCHEMA_METADATA(KEY STRING PRIMARY KEY, VALUE STRING);"))

;; (defun database-lookup-table-metadata (database key)
;;   (let* ((query (dbi:prepare database "SELECT VALUE FROM SCHEMA_METADATA WHERE KEY = ?;")))
;;     (dbi:execute query (list (symbol-name key)))
;;     (second (dbi:fetch query))))

;; (defun database-insert-table-metadata (database key value)
;;   (dbi:do-sql database
;;     "INSERT INTO SCHEMA_METADATA(KEY, VALUE) VALUES (?, ?);"
;;     (list (symbol-name key) value)))

;; (defun database-update-table-metadata (database key value)
;;   (dbi:do-sql database
;;     "UPDATE SCHEMA_METADATA SET VALUE = ? WHERE KEY = ?;"
;;     (list (symbol-name key) value)))

;; (defmethod marshal:class-persistent-slots ((class standard-object))
;;   (mop:ensure-finalized class)
;;   (mapcar #'mop:slot-definition-name (mop:class-slots class)))

;; (defgeneric database-create-or-alter-table-if-needed (class database))
;; (defmethod database-create-or-alter-table-if-needed ((class sql-table) database)
;;   (let ((key (class-name class)))
;;     (a:if-let (value (database-lookup-table-metadata database key))
;;       (progn 
;;         ;; check for difference
;;         (loop :for slot :in value
;;               :do )
;;         )

;;       (progn
;;         ;; create table
;;         (database-insert-table-metadata
;;          database key
;;          (format nil "~a" (marshal:marshal (mop:class-slots class)))
;;          )
;;         )
;;       ))
;;   )



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

(defgeneric database-create-table (class database &key if-not-exists))
(defmethod database-create-table ((class sql-table) database &key if-not-exists)
  (dbi:do-sql database (create-table-sql class :if-not-exists if-not-exists)))

(defgeneric database-drop-table (class database &key if-exists))
(defmethod database-drop-table ((class sql-table) database &key if-exists)
  (dbi:do-sql database (drop-table-sql class :if-exists if-exists)))



(defmethod insert-sql ((class sql-table) database instance)
  (let* ((slots (slots-to-insert class))
         (sql-names (mapcar #'sql-name slots)))
    (format nil "INSERT INTO ~a(~{~a~^, ~}) VALUES (~{~a~^, ~});"
            (sql-name class)
            sql-names
            (mapcar (constantly #\?) sql-names))))

;; (defmethod marshal:class-persistent-slots ((class standard-object))
;;   (mop:ensure-finalized (class-of class))
;;   (mapcar #'mop:slot-definition-name (mop:class-slots (class-of class)))
;;   ;; (remove-if-not (a:curry #'slot-boundp class)
;;   ;;                )
;;   )

(defun lisp-object->sql-object (type value)
  (assert (typep value type))
  (case type
    ((integer fixnum string boolean) value)
    ((symbol) (symbol-name value))
    (otherwise (format nil "~s" value))))

(defun sql-object->lisp-object (type value)
  (case type
    ((integer fixnum string boolean) value)
    ((symbol) (intern value))
    (otherwise
     (let ((*read-eval* nil))
       (read-from-string value)))))

(defmethod convert-to-sql-params-to-insert ((class sql-table) instance)
  (loop :for slot :in (slots-to-insert class)
        :for name = (mop:slot-definition-name slot)
        :for type = (mop:slot-definition-type slot)
        :collect
        (case type
          ((integer fixnum string boolean) (slot-value instance name))
          ((symbol) (symbol-name (slot-value instance name)))
          (otherwise (marshal:marshal (slot-value instance name))))))

(defmethod database-insert-slots ((class sql-table) database instance slots)
  (let ((slot-names (mapcar #'mop:slot-definition-name slots)))

    )
  )

;;;;; WAIT A SECOND
;;;;; IF A CLASS IS AUTO INCREMENT I KNOW WHETHER OR NOT TO INSERT THE KEY

(defmethod database-insert ((class sql-table) database instance)
  (let ((slots (mop:class-slots class)))
    (a:when-let (p (primary-key-slot class))
      (when (slot-value? instance (mop:slot-definition-name p))
        (a:removef slots p))
      (database-insert-slots class database instance slots))))

;; (defgeneric database-update-table-if-needed (class database class-slots-sql))
;; (defmethod database-update-table-if-needed ((class sql-table) database
;;                                             existing-class-slots-sql)
;;   (unless (equalp (class-slots-sql class) existing-class-slots-sql)
;;     (loop :for )
;;     ))

;;(defgeneric class-primary-key-slot-name (class))
;;(defmethod class-primary-key-slot-name ((class sql-table))
;;  (loop :for slot :in (mop:class-slots class)
;;        :when (primary-key slot)
;;          :return (mop:slot-definition-name slot)))
;;
;;(defgeneric database-insert (class instance database))
;;(defmethod database-insert ((class sql-table) instance database)
;;  
;;  (let ((primary-slot (class-primary-key-slot-name class)))
;;    (when (and (slot-boundp instance primary-slot)
;;               (slot-value instance primary-slot))
;;      (error "This instance already has a bound primary key"))
;;
;;    (let* ((slots (mop:class-slots class))
;;           (saved-slots (loop :for slot :in slots
;;                              :for name = (mop:slot-definition-name slot)
;;                              :when (and (slot-boundp instance name)
;;                                         (not (eq name primary-slot)))
;;                                :collect slot)) )
;;      (multiple-value-bind (sql params)
;;          (sxql:insert-into
;;              (sql-name class)
;;            (mapcar #'sql-name saved-slots)
;;            (mapcar (a:curry #'slot-value instance)
;;                    (mapcar #'mop:slot-definition-name saved-slots)))
;;        (dbi:do-sql database sql params)))))
;;
;;(defgeneric save-instance-of (class instance database))
;;(defmethod save-instance-of ((class sql-table) instance database)
;;
;;  ;; TODO create a utility function to get the primary key of a class
;;  (let ((primary-key-slot (mop:slot-definition-name
;;                           (first
;;                            (remove-if-not #'primary-key (mop:class-slots class))))))
;;    (if primary-key-slot
;;        t
;;
;;        ;; else
;;        (multiple-value-bind (sql-names slot-names)
;;            (loop :for slot :in (mop:class-slots class)
;;                  :for name = (mop:slot-definition-name slot)
;;                  :when (slot-boundp instance name)
;;                    :collect (sql-name slot) :into sql-names
;;                    :and :collect name :into slot-names
;;                  :finally (return (values sql-names slot-names)))
;;          (multiple-value-bind (sql data)
;;              (sxql:yield (sxql:insert-into (sql-name class)
;;                            sql-names
;;                            (mapcar (a:curry #'slot-value instance) slot-names)))
;;            (dbi:do-sql database sql data))))))
;;
;;(defun save-instance (instance database)
;;  ()
;;  )
;;
;;;; (defgeneric insert-into-table-sql (class))
;;;; (defmethod insert-into-table-sql ((class sql-table))
;;;;   (format nil "INSERT INTO ~a(~{~a~^,~}) VALUES(~{~a~^,~});"))
;;
;;
;;(defclass person (standard-sql-table)
;;  ((first-name :accessor first-name :initarg :first-name)
;;   (last-name :accessor last-name :initarg :last-name)
;;   (email :accessor email)
;;   (phone :accessor phone))
;;  (:metaclass sql-table))
;;
;;(defclass company (standard-sql-table)
;;  ((primary-contact :accessor primary-contact :references (person id)))
;;  (:metaclass sql-table))
;;
;;
;;(defparameter *bob-jones* (make-instance 'company))
;;
;;(print (references (second (mop:class-slots (class-of *person*)))))
