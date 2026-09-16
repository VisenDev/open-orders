(defpackage #:open-orders.main
  (:use #:cl
        #:open-orders.pagen
        #:open-orders.sql-table
        #:open-orders.tables
        #:open-orders.templates
        #:open-orders.auth)
  (:import-from #:url-rewrite
                #:url-encode)
  (:export
   #:main))
(in-package #:open-orders.main)


;;;; TODO: create a "query builder" abstraction that
;;;; builds the open-orders list table. Then we can
;;;; use that for the other tables or to let the user
;;;; do custom queries

(defvar *acceptor* nil)

(defparameter *toplevel-links*
  '("logout" "open-orders" "customers" "inventory"))

(defun insert-toplevel-links ()
  (table ()
    (tr ()
      (mapcar (lambda (arg)
                (td ()
                  (form (:action (format nil "/~a" arg))
                    (button (:type "submit")
                      arg))))
              *toplevel-links*))))

(hunchentoot:define-easy-handler (index :uri "/") ()
  (perform-auth-check)
  (hunchentoot:redirect "/open-orders"))

(defun get-open-orders-sorted-by (sort-by reverse)
  (let* ((raw (select-all 'open-order))
         (sorted 
           (if (null sort-by)
               raw
               ;; else
               (sort
                raw 
                (alexandria:switch (sort-by :test #'string=)
                  ("part-number"
                   (lambda (lhs rhs)
                     (string-lessp
                      (ignore-errors
                       (part-number (select 'part 'id (part lhs))))
                      (ignore-errors
                       (part-number (select 'part 'id (part rhs)))))))
                  ("purchase-order"
                   (lambda (lhs rhs)
                     (string-lessp (purchase-order lhs)
                                   (purchase-order rhs))))
                  ("due-date"
                   (lambda (lhs rhs)
                     (< (open-order-deadline lhs)
                        (open-order-deadline rhs))))
                  ("line-item"
                   (lambda (lhs rhs)
                     (< (line-item lhs)
                        (line-item rhs)))))))))
    (if reverse (reverse sorted) sorted)))

(defun get-customers-sorted-by (sort-by reverse)
  (let* ((raw (select-all 'customer))
         (sorted 
           (if (null sort-by)
               raw
               ;; else
               (sort
                raw 
                (alexandria:switch (sort-by :test #'string=)
                  ("name" (lambda (lhs rhs)
                            (string-lessp (name lhs)
                                          (name rhs))))
                  (otherwise (error "Unexpected sort-by")))))))
    (if reverse (reverse sorted) sorted)))

(hunchentoot:define-easy-handler (open-orders :uri "/open-orders") (sort-by reversed)

  (let ((reversed-p (string= reversed "true")))
    (flet ((table-header-href (sort-by)
             (format nil "/open-orders?sort-by=~a&reversed=~a"
                     sort-by
                     (if reversed-p "false" "true")))
           (edit-order-href (open-order-id)
             (format nil "/edit-order?id=~a" open-order-id)))
      (with-internal-page
        (insert-toplevel-links)
        (hr ())
        (table ()
          (tr ()
            (th () (a (:href (table-header-href "part-number")) "Part Number"))
            (th () (a (:href (table-header-href "purchase-order")) "Po Number"))
            (th () (a (:href (table-header-href "due-date")) "Due Date"))
            (th () (a (:href (table-header-href "line-item")) "Line Item"))
            (th () (a (:href "/new?type=order") (div (:class "border") "New"))))
          (mapcar (lambda (order)
                    (let ((href (edit-order-href (id order))))
                      (tr ()
                        (td ()
                          (a (:href href)
                            (ignore-errors
                             (part-number
                              (select 'part 'id (part order))))))
                        (td () (a (:href href)
                                 (purchase-order order)))
                        (td () (a (:href href) "TODO"))
                        (td () (a (:href href) (line-item order))))))
                  (get-open-orders-sorted-by sort-by reversed-p)))))))

(hunchentoot:define-easy-handler (new :uri "/new") (type)
  (with-internal-page 
    (alexandria:switch (type :test #'string=)
      ("order"
       (let ((id (insert (make-instance
                          'open-order
                          :purchase-order (open-orders.auth:auth-token-create)))))
         (hunchentoot:redirect (format nil "/edit-order?id=~a" id))))
      (otherwise
       (h1 () "Error: don't know how to make a new '~a'" type)))))

(defmacro when-assoc ((varname key alist) &body body)
  (alexandria:with-gensyms (keyval value)
    `(let* ((,keyval ,key)
            (,value (assoc ,key ,alist :test #'string-equal))
            (,varname (cdr ,value)))
       (declare (ignorable ,value ,keyval ,varname))
       (when ,varname
         ,@body))))

(defvar *previous-save-order-params* nil)
(hunchentoot:define-easy-handler (save-order :uri "/save-order") () 
  
  (let* ((params (hunchentoot:post-parameters*))
         (can-skip-update (equalp params *previous-save-order-params*))
         (id (parse-integer (hunchentoot:post-parameter "id"))))

    ;; only update database when params change
    (unless can-skip-update
      (setf *previous-save-order-params* params)
      (perform-auth-check)
      
      (let ((order (select 'open-order 'id id)))
        (when order
          ;; Customer Code
          (when-assoc (customer-code 'customer-code params)
            (let ((customer (select 'customer 'name customer-code)))
              (if customer
                  
                  ;;customer found, update id
                  (setf (customer order) (id customer))

                  ;; else create new customer
                  (let* ((new-customer (make-instance 'customer
                                                      :name customer-code))
                         (new-customer-id (insert new-customer)))
                    (setf (customer order) new-customer-id)))))

          ;; finally save order and redirect
          (update order))))

    ;; NOTE: we are using a switch here rather than encoding the
    ;; redirect url directly because EWW (Emacs Web Wowser) seems
    ;; to display the value of a form button as its label, which 
    ;; looks bad if the value is a long complex url
    (hunchentoot:redirect
     (alexandria:switch ((hunchentoot:post-parameter "redirect")
                         :test #'string-equal)
       ("open-orders" "/open-orders")
       ("customers" "/customers")
       ("inventory" "/inventory")
       ("logout" "/logout")
       ("po-details" (format nil "/edit-order?id=~a&tab=po-details" id))
       ("job-ticket" (format nil "/edit-order?id=~a&tab=job-ticket" id))
       ("shipping-details" (format nil "/edit-order?id=~a&tab=shipping-details" id))
       ("certificate-of-conformance"
        (format nil "/edit-order?id=~a&tab=certificate-of-conformance" id))
       (otherwise
        (hunchentoot:post-parameter "redirect")))
     :code 303)))

(hunchentoot:define-easy-handler (edit-order :uri "/edit-order") (id tab)

  (unless tab
      (hunchentoot:redirect (format nil "/edit-order?id=~a&tab=po-details" id)))

  (labels ((save-button (contents destination)
             (td ()
               (button (:type "submit"
                        :name "redirect"
                        :value destination)
                 contents))))
    (let ((order (select 'open-order 'id (parse-integer id))))
      (with-internal-page
        (form (:method "POST" :action "/save-order") 
          (input (:type "hidden" :name "id"
                  :value id))
          (table ()
            ;; toplevel tab bar
            (tr ()
              (mapcar (lambda (arg)
                        (save-button arg arg))
                      *toplevel-links*)))

          (hr ())

          ;; order tab bar
          (table ()
            (tr ()
              (mapcar (lambda (tab)
                        (save-button tab tab))
                      '("po-details" "job-ticket"
                        "shipping-details" "certificate-of-conformance"))))
          
          (table ()
            (tr ()
              (td ()
                (table ()
                  (tr ()
                    (td () "Customer Code")
                    (td ()
                      (input (:type "text" :name "customer-code"
                              :id "customer-code" :list "customer-code-list"
                              :value (ignore-errors
                                      (or
                                       (name (select 'customer 'id
                                                     (customer order)))
                                       "")))))
                    (datalist (:id "customer-code-list")
                      (mapcar #'name (select-all 'customer))))
                  (tr ()
                    (td () "Customer Name")
                    (save-button "View Customer Information"
                                        (format nil  "/edit-customer?id=~a"
                                                (customer order)))))))))))))

(hunchentoot:define-easy-handler (customers :uri "/customers") (sort-by reversed)

  (let ((reversed-p (string= reversed "true")))
    (flet ((table-header-href (sort-by)
             (format nil "/customers?sort-by=~a&reversed=~a"
                     sort-by
                     (if reversed-p "false" "true")))
           (edit-customer-href (open-customer-id)
             (format nil "/edit-customer?id=~a" open-customer-id)))
      (with-internal-page
        (insert-toplevel-links)
        (hr ())
        (table ()
          (tr ()
            (th () (a (:href (table-header-href "name")) "Name"))
            (th () (a (:href "/new?type=Customer") (div (:class "border") "New"))))
          
          (mapcar (lambda (customer)
                    (let ((href (edit-customer-href (getf customer :id))))
                      (tr ()
                        (td () (a (:href href) (getf customer :name))))))
                  (query 'customer (list (make-ref :slot 'name :as :name)
                                         (make-ref :slot 'id :as :id))
                         :sort-by
                         (cond ((string-equal sort-by "name") :name)
                               (t nil))
                         :predicate (lambda (lhs rhs)
                                      (if reversed-p
                                          (string-greaterp lhs rhs)
                                          (string-lessp lhs rhs))))))))))

(hunchentoot:define-easy-handler (inventory :uri "/inventory") ()
  (with-internal-page
    (insert-toplevel-links)
    (hr ())
    (p () "Inventory Page")
    (h3 () "Primary content goes here :)")))

(defun start ()
  (database-connect)
  (setf *acceptor* (make-instance 'hunchentoot:easy-acceptor :port 8000))
  (hunchentoot:start *acceptor*))

(defun stop ()
  (database-disconnect)
  (when *acceptor*
    (hunchentoot:stop *acceptor*)
    (setf *acceptor* nil)))


(defun main ()
  (start)
  (unwind-protect (loop (sleep 1))
    (stop)))
