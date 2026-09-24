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

(define-table customer
    ((field name
            :type string :initform "lorem ipsum"
            :metadata (:show-in-list-view-p t))
     (field primary-contact-name
            :type string :initform ""
            :metadata (:show-in-list-view-p t))
     (field (email phone)
            :type string :initform ""
            :metadata (:show-in-list-view-p t))))

(define-table order
    ((field po-number :type string :initform "lorem ipsum"
                      :metadata (:show-in-list-view-p t))
     (field customer :type string :initform ""
                     :metadata (:show-in-list-view-p t))
     (field customer-id :type integer :initform -1)
     (field part-number :type string :initform ""
                        :metadata (:show-in-list-view-p t))))

(defun geta (item alist &key (test #'equal))
  "Alist equivalent to getf"
  (cdr (assoc item alist :test test)))

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
       (pushnew (make-tab :name ,(format nil "[~a list]" (table-namestring def))
                          :url ,(format nil "/~a/list" (table-namestring def)))
                *toplevel-tabs* :test #'equalp)
       
       ;; new page
       (hunchentoot:define-easy-handler
           (,(open-orders.fn:symbolicate table-name '-new)
            :uri ,(format nil "/~a/new" (table-namestring def)))
           ()
         (perform-auth-check)

         (let* ((new (,(table-constructor def)))
                (id (,(table-set-function def) new)))
           (hunchentoot:redirect (format nil "/~a/edit?id=~a"
                                         ,(table-namestring def)
                                         id)
                                 :code 303)))
       
       ;; landing page
       (hunchentoot:define-easy-handler
           (,(open-orders.fn:symbolicate table-name '-list)
            :uri ,(format nil "/~a/list" (table-namestring def)))
           (sort-by reverse)
         (with-internal-page
           (hr ())
           (html-table ()
             (tr ()
               (td ()
                 (form (:action ,(format nil "/~a/new" (table-namestring def)))
                   (input (:type "submit"
                           :value ,(format nil "new ~a" (table-namestring def))))))))
           (hr ())
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
           (id)
         (let* ((params (hunchentoot:post-parameters*))
                (id (parse-integer id))
                (redirect-url (geta "redirect-url" params))
                (val (,(table-get-function def) id)))

           ;; iterate over all fields, getting thier values from
           ;; parameters and setting them when non-null
           ,@(mapcar (lambda (field)
                       `(let ((,(field-name field)
                                (geta ,(field-namestring field) params)))
                          (when ,(field-name field)
                            (setf (,(field-accessor field) val)
                                  ,(field-name field))))
                       )
                     (table-fields def))
           (,(table-set-function def) val)

           (hunchentoot:redirect redirect-url :code 303)
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
         (let ((,save-endpoint-var (format nil "/~a/save?id=~a"
                                           ,(table-namestring def)
                                           id))
               (,edit-value-var (,(table-get-function def) (parse-integer id))))
           (declare (ignorable ,save-endpoint-var ,edit-value-var))
           ,@edit-endpoint-definition)))))


;; Derivations
(derive-pages-from-table order (save-url order-value ())
  (with-internal-page
    (hr ())
    (p () "You've reached the edit page!")
    (p () (format nil "~a" order-value))))

(derive-pages-from-table customer (save-url val ())
  (with-internal-page
    (hr ())
    (form (:method "post" :action save-url)
      (html-table ()
        (tr ()
          (td ()
            (button (:type "submit" :name "redirect-url"
                     :value "/customer/list")
              "back"))
          (td ()
            (button (:type "submit" :name "redirect-url"
                     :value (hunchentoot:request-uri*))
              "save"))))
      (hr ())
      (html-table ()
        (tr ()
          (td () "Name:")
          (td () (input (:name "name" :value (customer-name val)))))
        (tr ()
          (td () "Primary-Contact:")
          (td () (input (:name "primary-contact-name"
                         :value (customer-primary-contact-name val)))))
        (tr ()
          (td () "Email:")
          (td () (input (:name "email"
                         :value (customer-email val)))))
        (tr ()
          (td () "Phone:")
          (td () (input (:name "phone"
                         :value (customer-phone val)))))))))

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
