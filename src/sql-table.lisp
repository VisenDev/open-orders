(defpackage #:open-orders.sql-table
  (:use #:cl)
  (:import-from #:trivial-types
                #:association-list)
  (:local-nicknames (#:a #:alexandria)
                    (#:mop #:closer-mop)))
(in-package #:open-orders.sql-table)


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

;; (defmethod slotd->column ((slotd sql-table-effective-slot-definition))
;;   (make-column
;;    :name (lisp-identifier->sql-identifier (mop:slot-definition-type slotd))
;;    :type (lisp-type->sql-type (mop:slot-definition-type slotd))
;;    :primary-key-p (primary-key slotd)
;;    :references (references slotd)
;;    :autoincrement-p (autoincrement slotd)))

(defmethod slotd-derive-primary-slot-p ((slotd mop:standard-effective-slot-definition))
  nil)

(defmethod slotd-derive-references ((slotd mop:standard-effective-slot-definition))
  nil)

(defmethod slotd-derive-autoincrement-p ((slotd mop:standard-effective-slot-definition))
  nil)

(defmethod slotd->column ((slotd mop:standard-effective-slot-definition))
  (make-column
   :name (lisp-name->sql-name (mop:slot-definition-type slotd))
   :type (lisp-type->sql-type (mop:slot-definition-type slotd))
   :primary-key-p (slotd-derive-primary-slot-p slotd)
   :references (slotd-derive-references slotd)
   :autoincrement-p (slotd-derive-autoincrement-p slotd)))

(defstruct table
  (name "" :type string)
  (slots nil :type list))

(defmethod class->table ((class standard-class))
  (make-table :name (lisp-name->sql-name (class-name class))
              :slots (mapcar #'slotd->column (mop:class-slots class))))



;;;; testing

(defclass person ()
  (id first-name last-name age))

(defmethod primary-key-p ((class person) slot-name)
  (eq slot-name 'id))


    
