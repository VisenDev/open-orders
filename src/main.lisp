(cl:defpackage #:open-orders.main
  (:use #:cl
        #:open-orders.fn
        #:open-orders.html-generator
        #:open-orders.database
        #:open-orders.derive-page
        #:open-orders.templates
        #:open-orders.auth
        )
  (:import-from #:url-rewrite
                #:url-encode)
  (:export
   #:main))
(in-package #:open-orders.main)

(defvar *acceptor* nil)

(fn (universal-time->date-string string) ((timestamp date))
  (multiple-value-bind (second minute hour
                        date month year day)
      (decode-universal-time timestamp)
    (declare (ignore second minute hour day))
    (format nil "~a ~a, ~a"
            (nth month
                 '("January"
                   "February"
                   "March"
                   "April"
                   "May"
                   "June"
                   "July"
                   "August"
                   "September"
                   "October"
                   "November"
                   "December"))
            date year)))

(define-table customer
    ((field name
            :type string :initform (open-orders.random:full-name)
            :metadata (:page-config
                       (page-config
                        :show-in-list-view-p t)))
     (field primary-contact-name
            :type string :initform (open-orders.random:full-name)
            :metadata (:page-config (page-config
                                     :show-in-list-view-p t)))
     (field address
            :type string :initform (format nil "~a ~a"
                                           (+ 100 (random 1000))
                                           (open-orders.random:street)))
     (field phone
            :type string :initform (format
                                    nil "~a"
                                    (open-orders.random:n-digit-number 10))
            :metadata (:page-config (page-config
                                     :show-in-list-view-p t)))
     (field email
            :type string :initform (format
                                    nil "~a@~a.com"
                                    (open-orders.random::first-name)
                                    (open-orders.random::last-name))
            :metadata (:page-config (page-config :show-in-list-view-p t)))))

(define-table inventory
  ((field (part-number location note last-updated)
          :type string :initform ""
          :metadata (:page-config
                     (page-config :show-in-list-view-p t)))))

(define-table employee
  ((field name
          :type string :initform (open-orders.random:full-name)
          :metadata (:page-config (page-config :show-in-list-view-p t)))
    (field (phone email)
          :type string :initform ""
          :metadata (:page-config (page-config :show-in-list-view-p t)))
   (field (birthday date-hired)
          :type date :initform (get-universal-time)
          :metadata (:page-config (page-config
                                   :show-in-list-view-p t
                                   :display-as universal-time->date-string)))))

(define-table purchase-order
  ((field (date-placed)
          :type date :initform (- (get-universal-time)
                                  (random 100000))
          :metadata (:page-config (page-config
                                   :show-in-list-view-p t
                                   :display-as universal-time->date-string)))
   (field supplier
          :type string :initform (open-orders.random:full-name)
          :metadata (:page-config (page-config :show-in-list-view-p t)))
   (field description
          :type string :initform ""
          :metadata (:page-config (page-config :show-in-list-view-p t)))))

(define-table po-details
    ((field customer-id :references customer
                        :metadata (:page-config
                                   (page-config :display-as customer-name
                                                :show-in-list-view-p t
                                                :display-name "customer")))
     (field purchase-order :type string
                           :initform (format
                                      nil "~a"
                                      (open-orders.random:n-digit-number
                                       (+ 4 (random 3))))
                           :metadata (:page-config
                                      (page-config :show-in-list-view-p t)))
     (field line-item :type integer :initform (random 5)
                      :metadata (:page-config (page-config
                                               :show-in-list-view-p t
                                               :compare-function <)))
     (field part-number :type string
                        :initform
            (format
             nil "~a"
             (open-orders.random:n-digit-number
              (+ 4 (random 3))))
                        :metadata (:page-config (page-config
                                                 :show-in-list-view-p t)))
     (field revision :type string :initform (open-orders.random:capital-letter))
     (field price-each :type string
                       :initform (format nil "~a.~a" (random 3)
                                         (open-orders.random:n-digit-number 2)))
     (field ship-terms :type string :initform "Freight Collect")
     (field billing-terms :type string :initform "Net30")
     (field material-type :type string :initform "")
     (field job-status :type string :initform "Waiting")
     (field notes :type string :initform ""
                  :metadata (:page-config (page-config
                                           :show-in-list-view-p t)))))

(defmacro derive-all-pages (table-name)
  `(progn
     (derive-list-page-from-table ,table-name)
     (derive-new-page-from-table ,table-name)
     (derive-save-page-from-table ,table-name)
     (derive-edit-page-from-table ,table-name)))

(pushnew (make-tab :name "<i>[logout]</i>" :url "/logout")
         *toplevel-tabs*
         :test #'equalp)
(derive-all-pages customer)
(derive-all-pages inventory)
(derive-all-pages employee)
(derive-all-pages purchase-order)
(derive-all-pages po-details)





(hunchentoot:define-easy-handler (home :uri "/") ()
  (hunchentoot:redirect (table-url (find-table 'po-details) "list")))

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
