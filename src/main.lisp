(cl:defpackage #:open-orders.main
  (:use #:cl
        #:open-orders.html-generator
        #:open-orders.database
        ;; #:open-orders.sql-table
        ;; #:open-orders.tables
        #:open-orders.templates
        #:open-orders.auth
        )
  (:import-from #:url-rewrite
                #:url-encode)
  (:export
   #:main))
(in-package #:open-orders.main)

(defvar *acceptor* nil)

(define-table order
    ((field po-number :type string :initform ""
                      :metadata (:show-in-list-view-p t))
     (field customer :type string :initform ""
                     :metadata (:show-in-list-view-p t))
     (field customer-id :type integer :initform -1))
  :id-field-metadata (:show-in-list-view-p t))

(defmacro derive-pages-from-table
    (table-name (save-endpoint-var edit-value-var edit-endpoint-parameters)
     &body
       edit-endpoint-definition)
  (let* ((def (gethash (symbol-name table-name) *tables*))
         (listed-fields (remove-if-not
                         (lambda (field)
                           (getf (field-metadata field) :show-in-list-view-p))
                         (table-fields def))))
    (assert def)
    `(progn
       ;; landing page
       (hunchentoot:define-easy-handler
           (,(open-orders.fn:symbolicate table-name '-list)
            :uri ,(format nil "/~a/list" (table-namestring def)))
           (sort-by reverse)
         (with-internal-page
           (hr ())
           (p () ,(format nil "~a" table-name))
           (html-table ()

             ;; table header
             (tr ()
               ,@(mapcar
                  (lambda (field)
                    `(th ()
                       (a (:href (format
                                  nil
                                  ,(format nil "/~a/list?sort-by=~a&reverse=~~a"
                                           (table-namestring def)
                                           (field-namestring field))
                                  (if (string= reverse "true") "false" "true")))
                         ',(field-name field))))
                  listed-fields))

             ;; table body
             (loop
               :for val
                 :across
                 (let* ((sorted (sort (open-orders.database::table-get-all
                                       *database-path* ',table-name)
                                      (lambda (a b)
                                        ;; todo, use the fields handler
                                        (string< (format nil "~a" a)
                                                 (format nil "~a" b)))
                                      :key
                                      (cond 
                                        ,@(mapcar
                                           (lambda (f)
                                             `((string= sort-by
                                                        ,(field-namestring f))
                                               (function ,(field-accessor f)))
                                             )
                                           (table-fields def)))))
                        (reversed (if (string= reverse "true")
                                      (nreverse sorted)
                                      sorted)))
                   reversed)
               :collect
               (tr ()
                 ,@(mapcar (lambda (field)
                             `(td ()
                                (a (:href (format nil "/~a/edit?id=~a"
                                                  ,(table-namestring def)
                                                  (,(table-id-accessor def) val)))
                                  (,(field-accessor field) val))))
                           listed-fields))))))
       
       ;; save endpoint
       (hunchentoot:define-easy-handler
           (,(open-orders.fn:symbolicate table-name '-save)
            :uri ,(format nil "/~a/save" (table-namestring def)))
           ()
         (let ((params (hunchentoot:post-parameters*)))
           (format nil "~a" params)
           )
         ;; get post parameters
         ;; perform save
         ;; redirect to redirect url from parameters
         )

       ;; edit endpoint
       (hunchentoot:define-easy-handler
           (,(open-orders.fn:symbolicate table-name '-edit)
            :uri (format nil "/~a/edit" ,(table-namestring def)))
           (id ,@edit-endpoint-parameters)
         (let ((,save-endpoint-var ,(format nil "/~a/save" table-name))
               (,edit-value-var (,(table-get-function def) (parse-integer id))))
           (declare (ignorable ,save-endpoint-var ,edit-value-var))
           ,@edit-endpoint-definition)))))

(derive-pages-from-table order (save-url order-value ())
  (with-internal-page
    (hr ())
    (p () "You've reached the edit page!")
    (p () (format nil "~a" order-value))))

(hunchentoot:define-easy-handler (home :uri "/") ()
  (hunchentoot:redirect "/order/list"))

;; (hunchentoot:define-easy-handler (orders :uri "/orders") ()
;;   (with-internal-page
;;     (hr ())
;;     (p () "Open Orders")
;;     (h3 () "Primary content goes here :)")
;;     (format nil "~a" (get-every-user))))

(defun start ()
  (setf *database-path* (asdf:system-relative-pathname "open-orders" "database/"))
  (setf *acceptor* (make-instance 'hunchentoot:easy-acceptor :port 8000))
  (hunchentoot:start *acceptor*))

(defun stop ()
  (when *acceptor*
    (hunchentoot:stop *acceptor*)
    (setf *acceptor* nil)))


(defun main ()
  (start)
  (unwind-protect (loop (sleep 1))
    (stop)))
