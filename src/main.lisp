(defpackage #:open-orders.main
  (:use #:cl #:clog)
  (:import-from #:defclass-std
                #:defclass/std
                #:class/std)
  (:local-nicknames (#:db #:open-orders.database))
  (:export #:main))
(in-package #:open-orders.main)

(deftype id () 'integer)

(defclass/std person ()
  ((first-name
    last-name
    email
    phone
    :type string)))

(defclass/std customer ()
  ((primary-contact :type id)
   (name :type string)))

(defclass/std line-item ()
  ((customer :type id)
   (order-number :type string)
   )
  )


(defvar *db* (make-instance 'db:database))
(defvar *db-path* "cl-db.database")

(defmacro callback (args &body body)
  `(lambda ,args (declare (ignorable ,@args))
     ,@body)
  )
 
(defmacro deftable (name &rest fields)
  (flet ((valid-sql-identifier-p (symbol)
           (every (lambda (ch) (or (eq ch #\_) (alphanumericp ch))) (symbol-name symbol))))
    (unless (valid-sql-identifier-p name)
      (error "Invalid sql table name: ~a" name))
    (let ((varname (intern (format nil "*SQL-CREATE-TABLE-~a*" (symbol-name name)))))
      `(defparameter ,varname (sxql:create-table ,name ,fields))
      )))

(defparameter *database-path* "database.sqlite3")
(defvar *database* nil)

(defun ensure-database-connected ()
  (unless *database*
    (setf *database* (dbi:connect :sqlite3 :database-name *database-path*))))

(defmacro do-sql (&body body)
  `(progn 
     (ensure-database-connected)
     (multiple-value-bind (sql params) (sxql:yield ,@body)
       (dbi:execute (dbi:prepare *database* sql) params))))

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

