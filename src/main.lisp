(defpackage #:cl-db.main
  (:use #:cl #:clog)
  (:export #:main #:create-tables))
(in-package #:cl-db.main)


;;; UTILS
(defmacro quick-defclass (name superclasses &body slots)
  (let* ((slot-forms nil))
    (loop :for slot :in slots
          :do (push
               `(,slot :initarg ,(intern (symbol-name slot) 'keyword) :accessor ,slot)
               slot-forms))
    `(defclass ,name ,superclasses ,slot-forms)
    )
  )

(defmacro callback (args &body body)
  `(lambda ,args (declare (ignorable ,@args))
     ,@body)
  )

(defparameter *tables* nil "list of the sql tables that have been defined in this program")

(defun valid-sql-identifer-p (symbol)
  (let ((str (symbol-name symbol)))
    (loop :for ch :across str
          :always (or (eq ch #\_) (alphanumericp ch)))))

(defmacro deftable (name &rest fields)
  (unless (valid-sql-identifer-p name)
    (error "Invalid sql table name: ~a" name))
  (let ((clos-fields (loop :for field :in fields
                           :for field-name = (first field)
                           :for type = (second (getf (cdr field) :type t))
                           :collect (list field-name
                                          :accessor field-name
                                          :type type
                                          :initarg (intern (symbol-name field-name) 'keyword))
                           :do (unless (valid-sql-identifer-p field-name)
                                 (error "Invalid sql field name: '~a~%" field-name))

                           ))
        (varname (intern (format nil "*SQL-CREATE-TABLE-~a*" (symbol-name name))))
        (classname (intern (format nil "~a-ROW" (symbol-name name)))))
    `(progn
       (push ,(list 'quote name) *tables*)
       (defclass ,classname ()
         ,clos-fields)
       (defparameter ,varname (sxql:create-table ,name ,fields))
       ))
  )


;;; IMPL
(defparameter *database-path* "database.sqlite3")
(defvar *database* nil)

(defun ensure-database-connected ()
  (unless *database*
    (setf *database* (dbi:connect :sqlite3 :database-name *database-path*))))

(defmacro do-sql (&body body)
  `(progn (format t "~a~%" ,@body)
          (ensure-database-connected)
          (multiple-value-bind (sql params) (sxql:yield ,@body)
            (dbi:execute (dbi:prepare *database* sql) params))))

(deftable customers
  (id :type 'integer :primary-key t :auto-increment t)
  (name :type 'text :not-null t)
  (primary_contact_name :type 'text)
  (primary_contact_email :type 'text))

(deftable orders
  (id :type 'integer :primary-key t :auto-increment t)
  (purchase_order_number :type 'text)
  (customer_id :type 'integer)          ;foreign key references customers
  (part_number :type 'text)
  (quantity :type 'integer)
  (due_date :type 'text)
  )

(defun create-tables ()
  (do-sql (sxql:drop-table :customers :if-exists t))
  (do-sql (sxql:drop-table :orders  :if-exists t))
  (do-sql *sql-create-table-customers*)
  (do-sql *sql-create-table-orders*)
  )

(defun list-open-orders (obj)
  (let* ((query (do-sql (sxql:select (:part_number :purchase_order_number :quantity :due_date)
                           (sxql:from :orders))))
         (table (clog:create-table obj))
         (heading (clog:create-table-row table)))
    (setf (background-color heading) "#dddddd")
    (create-table-heading heading :content "Part Number" :style "min-width:200px;")
    (create-table-heading heading :content "Purchase Order Number" :style "min-width:200px;")
    (create-table-heading heading :content "Quantity" :style "min-width:200px;")
    (create-table-heading heading :content "Due Date" :style "min-width:200px;")
    (loop :for row = (dbi:fetch query)
          :for i :from 0 :below 1000
          :while row
          :for table-row = (clog:create-table-row table)
          :do
             (when (oddp i)
               (setf (background-color table-row) "#eeeeee"))
             (create-table-column table-row :content (getf row :|part_number|))
             (create-table-column table-row :content (getf row :|purchase_order_number|))
             (create-table-column table-row :content (getf row :|quantity|))
             (create-table-column table-row :content (getf row :|due_date|)))))

(defun insert-random-order ()
  (do-sql
    (sxql:insert-into :orders
      (sxql:set= 
       :purchase_order_number (random 10000)
       :part_number (random 100000)
       :quantity (* 100 (random 100))
       :due_date (format nil "~a-~a-~a" (random 10) (random 28) (+ 2025 (random 3))))
      )))

(defun on-new-window (body)
  (ensure-database-connected)
  (insert-random-order)
  (setf (title (html-document body)) "Open Orders")
  (let* ((menu (create-div body))
         (contents (create-div body :style "display: flex; flex-direction: row"))
         (display-panel (create-div contents))
         (edit-panel (create-div contents))
         (home-tab (create-button menu :content "Home" :style "font-weight:bold;"))
         (open-orders-tab (create-button menu :content "Open Orders"))
         (clean-tab (create-button menu :content "Clean"))
         )
    (set-on-click (create-button edit-panel :content "Click to add new order")
                  (lambda (obj)
                    (declare (ignore obj))
                    (insert-random-order)
                    (destroy-children display-panel)
                    (list-open-orders display-panel)))
    (create-p edit-panel :content *tables*)
    (list-open-orders display-panel)
    (set-on-click home-tab (callback (obj) (create-section display-panel :p :content "clicked home tab!")))
    (set-on-click open-orders-tab (callback (obj) (create-section display-panel :p :content "licked open-orders-tab")))
    (set-on-click clean-tab (callback (obj) (destroy-children display-panel)))
    ))

(defun main()
  (initialize #'on-new-window)
  (open-browser)
  )
