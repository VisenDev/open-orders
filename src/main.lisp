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

(defmacro deftable (name &rest fields)
  (flet ((valid-sql-identifier-p (symbol)
           (every (lambda (ch) (or (eq ch #\_) (alphanumericp ch))) (symbol-name symbol))))
    (unless (valid-sql-identifier-p name)
      (error "Invalid sql table name: ~a" name))
    (let ((clos-fields (loop :for field :in fields
                             :for field-name = (first field)
                             :for type = (second (getf (cdr field) :type t))
                             :collect (list field-name
                                            :accessor field-name
                                            :type type
                                            :initarg (intern (symbol-name field-name) 'keyword))
                             :do (unless (valid-sql-identifier-p field-name)
                                   (error "Invalid sql field name: '~a~%" field-name))

                             ))
          (varname (intern (format nil "*SQL-CREATE-TABLE-~a*" (symbol-name name))))
          (classname (intern (format nil "~a-ROW" (symbol-name name)))))
      `(progn
         (push ,(list 'quote name) *tables*)
         (defclass ,classname ()
           ,clos-fields)
         (defparameter ,varname (sxql:create-table ,name ,fields))
         ))))


;;;
(defparameter *database-path* "database.sqlite3")
(defvar *database* nil)

(defun ensure-database-connected ()
  (unless *database*
    (setf *database* (dbi:connect :sqlite3 :database-name *database-path*))))

(defmacro do-sql (&body body)
  `(progn ;;c (format t "~a~%" ,@body)
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

(defun text-with-tooltip (text tooltip)
  (format nil "<div title=\"~a\">~a</div>" tooltip text))

(deftype app-page () '(member :open-orders :customers))
(defclass app ()
  ((page :accessor page :initform :open-orders :type app-page)))

(defun on-new-window (body)
  (setf (connection-data-item body "app") (make-instance 'app))
  (let* ((font-size "11pt")
         (pressed (rgb 220 220 220))
         (hover (rgb 240 230 230))
         (sb (create-style-block body)))
    (add-style sb :class "pressed" `(("background-color" ,pressed)
                                     ("text-shadow" ("0.5px" "0" "0" "black"))))
    (add-style sb :element "body" `(("color" :black)
                                    ("background-color" :white)
                                    ("border" :none)
                                    ("padding" 0)
                                    ("margin" 0)
                                    ("display" :flex)
                                    ("flex-direction" :column)
                                    ("font-family" "arial")
                                    ("font-size" ,font-size)))
    (add-style sb :element "button"       '(("color"           :black)
                                            ("text-decoration" :none)
                                            ("border"          :solid)
                                            ("border-width" "1px")
                                            ("background" :white)
                                            ("font-size" "100%")))
    (add-style sb :element "button:hover" `(("background-color" ,hover)))
    (add-style sb :element "th" `(("padding " "0px")
                                  ("font-weight" "normal")
                                  ("font-size" ,font-size))))

  (ensure-database-connected)
  (create-tables)
  (insert-random-order)
  (setf (title (html-document body)) "Open Orders")
  (symbol-macrolet ((app (connection-data-item body "app")))
    (let* ((menu (create-div body))
           (contents (create-div body :style "display: flex; flex-direction: row"))
           (display-panel (create-div contents :content (page app)))
           (edit-panel (create-div contents))
           (open-orders-tab (create-button menu :content (text-with-tooltip
                                                          "Open Orders"
                                                          "Swap to open orders page")
                                           :class "pressed"))
           (customers-tab (create-button menu :content (text-with-tooltip
                                                        "Customers" "Swap to customers page")))
           )
      (declare (ignorable display-panel edit-panel))
      (link-slot-to-element app page display-panel)
      (defmethod (setf page) :after (new-page (app app))
        (remove-class open-orders-tab "pressed")
        (remove-class customers-tab "pressed")
        (ecase new-page
          (:customers (add-class customers-tab "pressed"))
          (:open-orders (add-class open-orders-tab "pressed")))
        )

      (set-on-click open-orders-tab
                    (callback (obj)
                      (setf (page app) :open-orders)))
      (set-on-click customers-tab
                    (callback (obj)
                      (setf (page app) :customers)))))

    )

(defun main()
  (initialize #'on-new-window)
  (open-browser)
  )
