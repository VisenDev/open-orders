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

(defun create-tables ()
  (do-sql (sxql:drop-table :customers :if-exists t))
  (do-sql (sxql:drop-table :orders  :if-exists t))
  (do-sql
    (sxql:create-table customers
        ((id :type 'integer
             :primary-key t
             :auto-increment t)
         (name :type 'text
               :not-null t))))
  (do-sql
    (sxql:create-table orders
        ((id :type 'integer
             :primary-key t
             :auto-increment t)
         (purchase_order_number :type 'text)
         (customer_id :type 'integer)   ;foreign key references customers
         (part_number :type 'text)
         (quantity :type 'integer)
         (due_date :type 'text))))
  )

(defun list-open-orders (obj)
  (let ((query (do-sql (sxql:select (:part_number :purchase_order_number :quantity :due_date)
                           (sxql:from :orders)))))
    (loop :for row = (dbi:fetch query)
          :for i :from 0 :below 1000
          :while row
          :do (create-p obj :content (format nil "ROW ~a: ~a" i row)))))

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
         (contents (create-div body))
         (home-tab (create-button menu :content "home"))
         (open-orders-tab (create-button menu :content "open-orders"))
         (clean-tab (create-button menu :content "clean"))
         )
    (list-open-orders contents)
    (set-on-click home-tab (callback (obj) (create-section contents :p :content "clicked home tab!")))
    (set-on-click open-orders-tab (callback (obj) (create-section contents :p :content "licked open-orders-tab")))
    (set-on-click clean-tab (callback (obj) (destroy-children contents)))
    ))

(defun main()
  (initialize #'on-new-window)
  (open-browser)
  )
