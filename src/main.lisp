(defpackage #:open-orders.main
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


;;;; IMPORTANT
;;;; Setting catch-errors-p to nil enables the lisp debugger
;;;; This should be set to t when running in production
(setf hunchentoot:*catch-errors-p* t)

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
  ((field part-number
          :type string :initform (format nil "~a"
                                         (open-orders.random:n-digit-number 8))
          :metadata (:page-config
                     (page-config :show-in-list-view-p t)))
   (field (location note)
          :type string :initform ""
          :metadata (:page-config
                     (page-config :show-in-list-view-p t)))
   (field last-updated
          :type date :initform (get-universal-time)
          :metadata (:page-config
                     (page-config :show-in-list-view-p t
                                  :display-as universal-time->date-string
                                  :compare-function <)))))

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
                                   :compare-function <
                                   :display-as universal-time->date-string)))))

(define-table purchase-order
  ((field (date-placed)
          :type date :initform (- (get-universal-time)
                                  (random 100000))
          :metadata (:page-config (page-config
                                   :show-in-list-view-p t
                                   :display-as universal-time->date-string
                                   :compare-function <)))
   (field supplier
          :type string :initform (open-orders.random:full-name)
          :metadata (:page-config (page-config :show-in-list-view-p t)))
   (field description
          :type string :initform ""
          :metadata (:page-config (page-config :show-in-list-view-p t)))))

(define-table po-details
    ((field part-number :type string
                        :initform
            (format
             nil "~a"
             (open-orders.random:n-digit-number
              (+ 4 (random 3))))
                        :metadata (:page-config (page-config
                                                 :show-in-list-view-p t)))
     (field customer-id :references customer
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

     (field revision :type string :initform (open-orders.random:capital-letter)
            :metadata (:page-config (page-config
                                     :suggested-values ("A" "B" "C" "D" "E" "F"))))
     (field price-each :type string
                       :initform (format nil "~a.~a" (random 3)
                                         (open-orders.random:n-digit-number 2)))
     (field ship-terms :type string :initform "Freight Collect"
                       :metadata (:page-config
                                  (page-config :suggested-values ("Freight Collect"
                                                                  "Prepay And Add"
                                                                  "Pickup"))))
     (field billing-terms :type string :initform "Net30"
            :metadata (:page-config (page-config
                                     :suggested-values ("Net20" "Net30" "Net60"
                                                                "Net90"))))
     (field material-type :type string :initform "")
     (field job-status :type string :initform "Waiting"
                       :metadata (:page-config (page-config
                                                :suggested-values ("Running"
                                                                   "In Stock"
                                                                   "In Setup"
                                                                   "Waiting"))))
     (field notes :type string :initform ""
                  :metadata (:page-config (page-config
                                           :show-in-list-view-p t
                                           :suggested-values ("Wess Part"))))))

(defun derive-all-pages (table-name)
  (derive-list-page-from-table table-name)
  (derive-new-page-from-table table-name)
  (derive-delete-page-from-table table-name)
  (derive-save-page-from-table table-name)
  (derive-edit-page-from-table table-name)
  (derive-view-reference-page-from-table table-name))

(pushnew (make-tab :name "<i>[logout]</i>" :url "/logout")
         *toplevel-tabs*
         :test #'equalp)
(derive-all-pages 'customer)
(derive-all-pages 'inventory)
(derive-all-pages 'employee)
(derive-all-pages 'purchase-order)
(derive-all-pages 'po-details)

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
