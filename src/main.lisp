(defpackage #:open-orders.main
  (:use #:cl #:clog)
  (:import-from #:defclass-std
                #:defclass/std
                #:class/std)
  (:local-nicknames (#:db #:open-orders.database)
                    (#:a #:alexandria))
  (:export #:main))
(in-package #:open-orders.main)

;; (deftype id () 'integer)

;; (defclass/std person ()
;;   ((first-name
;;     last-name
;;     email
;;     phone
;;     :type string)))

;; (defclass/std customer ()
;;   ((primary-contact :type id)
;;    (name :type string)))

;; (defclass/std line-item ()
;;   ((customer :type id)
;;    (order-number :type string)
;;    )
;;   )

(defmacro do-sql (database &body body)
  (a:with-gensyms (query)
    `(multiple-value-bind (sql params) (sxql:yield ,@body)
       (let ((,query (dbi:prepare ,database sql)))
         (dbi:execute ,query params)
         ,query))))

(class/std table-definition sql forms)

(defvar *tables* (make-hash-table)
  "What the sql tables should look like")
(defvar *active-table-definitions* (make-hash-table)
  "The actually active definitions for sql tables")

(eval-when (:compile-toplevel)
  (defun valid-sql-identifier-p (symbol)
    (every (lambda (ch) (or (char= #\_ ch)
                            (alphanumericp ch)))
           (symbol-name symbol))))

;; TODO create generic object insert/get/set functions rather than
;; definining specific macros for each object

(defmacro deftable (name &body |(name type)|)
  (let* ((body |(name type)|)
         (sql-rows (loop :for (name type) :in body
                         :collect `(,name :type ',type)))
         (class-rows (loop :for (name type) :in body
                           :collect `(,name :type ,(if (eq type 'blob) t type))))
         (field-name-keywords
           (mapcar
            (a:compose #'a:make-keyword #'first)
            body)))
    (unless (every (a:compose #'valid-sql-identifier-p #'first) body)
      (error "Your table has an invalid sql identifier as a field name"))
    
    `(progn

       ;; record sql
       (setf (gethash ',name *tables*)
             (make-instance 'table-definition
                            :sql (sxql:create-table
                                     ,name
                                     ,(cons '(id :type 'integer
                                              :auto-increment t
                                              :primary-key t)
                                            sql-rows))
                            :forms ',body))
       ;; record class definition
       (defclass/std ,name () ,(cons '(id :type integer) class-rows))

       ;; record database get function
       (defun ,(intern (format nil "DATABASE-~a-GET" name)) (database id)
         (let* ((query
                  (do-sql database
                    (sxql:select
                        ,field-name-keywords
                      (sxql:from (,(a:make-keyword name)))
                      (sxql:where (:= :id id)))))
                (results (dbi:fetch query))
                (instance (make-instance ',name)))
           ,@(loop :for (name type) :in body
                   :for i :from 0
                   :collect `(setf (slot-value instance ',name)
                                   (nth ,i results)))
           instance))
       
       ;; record database set function
       (defun ,(intern (format nil "DATABASE-~a-SET" name))
           (database ,name)
         (do-sql database
           (sxql:update ,name
             ',field-name-keywords
             (list ,@(mapcar (lambda (def)
                               `(slot-value ,name ',(first def)))
                             (cons '(id integer) body)))
             (sxql:where (:= :id (slot-value ,name 'id))))))

       ;; record database insert new function
       (defun ,(a:symbolicate 'database- name '-insert) (database ,name)
         (first (dbi:fetch
                 (do-sql database
                   (sxql:insert-into ,name
                     ,field-name-keywords
                     (list ,@(mapcar (lambda (def)
                                       `(ignore-errors
                                         (slot-value ,name ',(first def))))
                                     body))
                     (sxql:returning :id)))))))))



(defun update-table-schema (database table-name old-definition-forms new-definition-forms)
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
     (a:if-let (old (gethash k *active-table-definitions*))
       (progn
         (update-table-schema database k (forms old) (forms v))
         (setf (gethash k *active-table-definitions*) v))
       (progn
         (do-sql database (sxql:drop-table k :if-exists t))
         (do-sql database (sql v)))))
   *tables*))


(deftable person
  (first_name string)
  (last_name string)
  (notes string)
  (email string)
  (phone string))

(deftable customer
  (name string)
  (primary_contact integer)) ;; refererences person

(deftable open_order
  (customer integer) ;;references customer
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

