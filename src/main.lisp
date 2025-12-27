(defpackage #:cl-db.main
  (:use #:cl #:clog #:clog-gui)
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

(defun open-orders (obj)
  (let ((win (create-gui-window obj :title "Count")))
    (dotimes (n 100)
      ;; window-content is the root element for the clog-gui
      ;; windows
      (create-div (window-content win) :content n))))

(defun on-help-about (obj)
  (let* ((about (create-gui-window obj
                                   :title   "About"
                                   :content "<div class='w3-black'>
                                         <center><img src='/img/clogwicon.png'></center>
                                         <center>Campro Open Orders</center>
                                         <center><i>Made using CLOG</i></center></div>
                                         <div><p><center>Code by</center>
                                         <center>(c) 2025 - Wess Burnett</center></p></div>"
                                   :hidden  t
                                   :width   200
                                   :height  215)))
    (window-center about)
    (setf (visiblep about) t)
    (set-on-window-can-size about (lambda (obj)
                                    (declare (ignore obj))()))))

(defun on-new-window (body)
  (setf (connection-data-item body "app") (make-instance 'app))
  (clog-gui-initialize body)
  (enable-clog-popup)                   ; To allow browser popups
  (add-class body "w3-cyan")

  (with-clog-create body
      (gui-menu-bar
       ()
       (gui-menu-icon (:on-click 'on-help-about))
       (gui-menu-item (:content "Open Orders" :on-click 'open-orders))
       (gui-menu-drop-down (:content "File")
                           (gui-menu-item (:content "Count" :on-click 'on-file-count))
                           (gui-menu-item (:content "Browse" :on-click 'on-file-browse))
                           (gui-menu-item (:content "Drawing" :on-click 'on-file-drawing))
                           (gui-menu-item (:content "Movie" :on-click 'on-file-movies))
                           (gui-menu-item (:content "Pinned" :on-click 'on-file-pinned)))
       (gui-menu-drop-down (:content "Window")
                           (gui-menu-item (:content "Maximize All" :on-click 'maximize-all-windows))
                           (gui-menu-item (:content "Normalize All" :on-click 'normalize-all-windows))
                           (gui-menu-window-select ()))
       (gui-menu-drop-down (:content "Dialogs")
                           (gui-menu-item (:content "Alert Dialog Box" :on-click 'on-dlg-alert))
                           (gui-menu-item (:content "Input Dialog Box" :on-click 'on-dlg-input))
                           (gui-menu-item (:content "Confirm Dialog Box" :on-click 'on-dlg-confirm))
                           (gui-menu-item (:content "Form Dialog Box" :on-click 'on-dlg-form))
                           (gui-menu-item (:content "Server File Dialog Box" :on-click 'on-dlg-file)))
       (gui-menu-drop-down (:content "Toasts")
                           (gui-menu-item (:content "Alert Toast" :on-click 'on-toast-alert))
                           (gui-menu-item (:content "Warning Toast" :on-click 'on-toast-warn))
                           (gui-menu-item (:content "Success Toast" :on-click 'on-toast-success)))
       (gui-menu-drop-down (:content "Help")
                           (gui-menu-item (:content "About" :on-click 'on-help-about)))
       (gui-menu-full-screen ())))

  )

(defun main()
  (initialize #'on-new-window)
  (open-browser)
  )
