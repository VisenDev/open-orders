(defpackage #:open-orders.main
  (:use #:cl #:clog)
  (:import-from #:defclass-std
                #:defclass/std
                #:class/std)
  (:local-nicknames (#:db #:open-orders.database)
                    (#:a #:alexandria))
  (:export #:main))
(in-package #:open-orders.main)

(defmacro do-sql (database &body body)
  (a:with-gensyms (query)
    `(multiple-value-bind (sql params) (sxql:yield ,@body)
       (let ((,query (dbi:prepare ,database sql)))
         (dbi:execute ,query params)
         ,query))))

(class/std table-definition create-table-sql raw-input-forms)

(defvar *tables* (make-hash-table)
  "What the sql tables should look like")
(defvar *active-tables* (make-hash-table)
  "The actually active definitions for sql tables")

(eval-when (:compile-toplevel)
  (defun valid-sql-identifier-p (symbol)
    (every (lambda (ch) (or (char= #\_ ch)
                            (alphanumericp ch)))
           (symbol-name symbol)))

  ;; TODO create generic object insert/get/set functions rather than
  ;; definining specific macros for each object

  (defun column-definition->sql-type (def)
    (let ((type (second def)))
      (cond ((and (listp type)
                  (cl:string-equal 'foreign-key (first type)))
             'integer)
            (t type))))

  (defun column-definition->sql-option (def)
    (let ((type (second def))
          (name (first def)))
      (if (and (listp type)
               (string-equal (first type) 'foreign-key))
          `(sxql:foreign-key '(,name) :references '(,(second type) id))
          nil)))

  (defun column-definition->sql-column-definition (def)
    (list (first def) :type (list 'quote (column-definition->sql-type def))))

  (defun column-definition->lisp-type (def)
    (let ((type (second def)))
      (cond ((and (listp type)
                  (cl:string-equal 'foreign-key (first type)))
             'integer)
            ((string-equal type 'blob)
             't)
            (t type))))

  (defun column-definition->class-slot-definition (def)
    (list (first def) :type (column-definition->lisp-type def))))

;; (defun generic-database-get-function (database table/class-name field/slot-names id)
;;   (let* ((all-slots (cons 'id field/slot-names))
;;          (query (do-sql database (sxql:select all-slots
;;                                     (sxql:where (:= :id id))
;;                                    (sxql:from table/class-name))))
;;          (results (dbi:fetch query)))
;;     results
;;     )
;;   )


;; (defun make-generic-database-insert-function (name columns)
;;   `(defun ,(a:symbolicate 'database-insert- name) (database id)
;;      (do-sql database
;;        (sxql:select ,(cons 'id (mapcar #'first columns))
;;          (sxql:where (:= id id))
;;          (sxql:)))
;;      )
;;   )

(defun make-generic-database-get-function (name columns)
  (let ((name name)
        (columns columns))
    (lambda (database id)
      (dbi:do-sql database
        (format nil "SELECT ~[~a~^,~] FROM ~a WHERE id = ~a"
                (cons 'id (mapcar #'first columns))
                name
                id)))))
  


(defmacro deftable (name &body columns)
  "Columns should be in the form (name type). Types may be either a simple type or a
   compound type of the form (foreign-key <referenced-table>)."
  (dolist (field-name (mapcar #'first columns))
    (unless (valid-sql-identifier-p field-name)
      (error "~a is not a valid sql identifier" field-name)))
  (unless (valid-sql-identifier-p name)
    (error "Table name ~a is not a valid sql table name" name))

  (let* ((create-table-sql
           `(sxql:create-table ,name
                ,(cons
                  '(id :type 'integer :primary-key t :auto-increment t)
                  (mapcar #'column-definition->sql-column-definition columns))
              ,@(remove nil (mapcar #'column-definition->sql-option columns))))
         (class-slots
           (cons '(id :type integer)
                 (mapcar #'column-definition->class-slot-definition columns))))
    `(progn
       ;; record sql
       (setf (gethash ',name *tables*)
             (make-instance 'table-definition
                            :raw-input-forms ',columns
                            :create-table-sql ,create-table-sql))
       
       ;; record class definition
       (defclass/std ,name () ,class-slots))))



(defun update-table-schema
    (database table-name old-definition-forms new-definition-forms)
  "Updates an existing sql table to follow a new schema"
  (let ((to-delete
          (loop :for def :in old-definition-forms
                :unless (member def new-definition-forms :test 'equalp)
                  :collect def))
        (to-add 
          (loop :for def :in new-definition-forms
                :unless (member def old-definition-forms :test 'equalp)
                  :collect def)))
    (loop :for (name type) :in to-delete
          :do (do-sql database
                (sxql:alter-table table-name (sxql:drop-column name))))
    (loop :for (name type) :in to-add
          :do (do-sql database
                (sxql:alter-table table-name (sxql:add-column name :type `',type))))))

(defun ensure-tables-exist (database)
  (maphash 
   (lambda (k v)
     (a:if-let (old (gethash k *active-tables*))
       (progn
         (update-table-schema database k (raw-input-forms old) (raw-input-forms v))
         (setf (gethash k *active-tables*) v))
       (progn
         (do-sql database (sxql:drop-table k :if-exists t))
         (do-sql database (create-table-sql v)))))
   *tables*))


(deftable person
  (first_name string)
  (last_name string)
  (notes string)
  (email string)
  (phone string))

(deftable customer
  (name string)
  (primary_contact (foreign-key person)))

(deftable open_order
  (customer (foreign-key customer))
  (order_number string)
  (part_number string)
  (description string)
  (revision string)
  (price string)
  (deliveries blob)
  (shipping_terms string)
  (payment_terms string)
  (notes string))

;; (defvar *db* (make-instance 'db:database))
;; (defvar *db-path* "cl-db.database")

(defmacro callback (args &body body)
  `(lambda ,args (declare (ignorable ,@args))
     ,@body)
  )
 
;; (defmacro deftable (name &rest fields)
;;   (flet ((valid-sql-identifier-p (symbol)
;;            (every (lambda (ch) (or (eq ch #\_) (alphanumericp ch))) (symbol-name symbol))))
;;     (unless (valid-sql-identifier-p name)
;;       (error "Invalid sql table name: ~a" name))
;;     (let ((varname (intern (format nil "*SQL-CREATE-TABLE-~a*" (symbol-name name)))))
;;       `(defparameter ,varname (sxql:create-table ,name ,fields))
;;       )))


(defparameter *quit* nil
  "When true the executable should quit")

(defun on-new-window (body)
  (clog:set-html-on-close body "<script>close();</script>")
  (setf (clog:title (clog:html-document body)) "Campro")
  ;; (clog:clog-gui-initialize body)
  (clog:enable-clog-popup)                   ; To allow browser popups
  ;; (add-class body "w3-cyan")



  ;; Block until body has been closed
  (clog:run body)

  ;; Quit the executable  
  (setf *quit* t)
  )

(defun main ()
  (setf *quit* nil)
  (ignore-errors
   (clog:shutdown))
  (clog:initialize #'on-new-window)
  (clog:open-browser)
  (loop
   :until *quit*
   :do
      (sleep 1))
  (clog:shutdown)
  )

