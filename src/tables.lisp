(defpackage #:open-orders.tables
  (:use #:cl
        #:open-orders.fn
        #:open-orders.html-generator
        #:open-orders.database
        #:open-orders.derive-page
        #:open-orders.templates
        #:open-orders.auth)
  (:export
   #:customer
   #:make-customer
   #:customer-p
   #:copy-customer
   #:customer-id
   #:customer-name
   #:customer-primary-contact-name
   #:customer-address
   #:customer-phone
   #:customer-email
   #:inventory
   #:make-inventory
   #:inventory-p
   #:copy-inventory
   #:inventory-id
   #:inventory-part-number
   #:inventory-note
   #:inventory-location
   #:inventory-last-updated
   #:purchase-order
   #:make-purchase-order
   #:purchase-order-p
   #:copy-purchase-order
   #:purchase-order-id
   #:purchase-order-date-placed
   #:purchase-order-supplier
   #:purchase-order-description
   #:po-details
   #:make-po-details
   #:po-details-p
   #:copy-po-details
   #:po-details-id
   #:po-details-part-number
   #:po-details-customer-id
   #:po-details-purchase-order
   #:po-details-line-item
   #:po-details-revision
   #:po-details-price-each
   #:po-details-ship-terms
   #:po-details-billing-terms
   #:po-details-material-type
   #:po-details-job-status
   #:po-details-notes
   #:po-details-release-schedule
   #:employee
   #:make-employee
   #:employee-p
   #:copy-employee
   #:employee-id
   #:employee-name
   #:employee-email
   #:employee-phone
   #:employee-date-hired
   #:employee-birthday
   #:universal-time->date-string
   #:get-customer))
(in-package #:open-orders.tables)

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

(defstruct shipment date amount)

(defun po-details-decode-release-schedule (parameters-alist po-details field)
  (loop
    :with field-namestring = (field-namestring field)
    :with field-namestring-len = (length field-namestring)
    :with results = (make-hash-table)
    :for (param-name . param-value) :in parameters-alist
    :for (param-namestring param-index param-subfieldname)
      = (uiop:split-string param-name :separator '(#\|))
    :when (string= param-namestring field-namestring)
      :do (push (cons param-subfieldname param-value)
      (gethash (parse-integer param-index) results))
    :finally (setf (po-details-release-schedule po-details)
                   (mapcar
                    (lambda (shipment-alist)
                      (make-shipment :date (or (ignore-errors
                                                (http-form-datestring->universal-time
                                                 (geta "date" shipment-alist)))
                                               0)
                                     :amount (parse-integer 
                                              (geta "amount" shipment-alist)
                                              :junk-allowed t)))
                    (mapcar #'cdr
                            (sort (loop :for index :being :the :hash-keys :of results
                                          :using (hash-value val)
                                        :collect (cons index val))
                                  #'<
                                  :key #'car))))))

(defun generate-release-schedule-edit-ui (id def field value)
  (let* ((mobilep (mobile-browser-p))
         (add-row-button
           (button
               (:type "submit"
                :name "redirect-url"

                ;; TODO figure out how to make this save
                :value
                (table-url
                 def "set-field"
                 (cons :id id)
                 (cons :field-namestring
                       (field-namestring field))
                 (cons :value
                       (let ((*package* (find-package 'cl)))
                         (format nil "~S"
                                 (cons
                                  (make-shipment
                                   :amount 1000
                                   :date (get-universal-time))
                                  value))))
                 (cons :redirect-url
                       (hunchentoot:request-uri*)))
)
             "Add Row")))
    (list
     (if mobilep
         (list (tr () (td () "<i>Release Schedule</i>"))
               (tr () (td () add-row-button))
               (tr () (td () (hr ()))))
         (tr ()
           (td ()  "<i>Release Schedule</i>")
           (td () add-row-button)))
     (if mobilep ""
         (tr ()
           (th () "Date")
           (th () "Amount")))
     (loop
       :for shipment :in (sort value #'< :key #'shipment-date)
       :for i :from 0
       :for date
         = (input (:value (multiple-value-bind
                                (second minute hour date month year)
                              (decode-universal-time
                               (shipment-date shipment))
                            (declare (ignore second minute hour))
                            (format nil "~a-~2,'0d-~2,'0d" year month date))
                   :name (format nil "~a|~a|date"
                                 (field-namestring field)
                                 i)
                   :type "date"))
       :for amount
         = (input (:value (shipment-amount shipment)
                   :name (format nil "~a|~a|amount"
                                 (field-namestring field)
                                 i)))
       :if mobilep
         :collect 
         (list (tr () (td () date))
               (tr () (td () amount))
               (tr () (td () (hr ()))))
       :else
         :collect (tr ()
                    (td () date)
                    (td () amount))
       :end))))

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
                                           :suggested-values ("Wess Part"))))
     (field
      release-schedule
      :type list
      :metadata
      (:page-config
       (page-config
        :edit-ui-generator
        generate-release-schedule-edit-ui
        :http-parameter-decoding-function
        po-details-decode-release-schedule)))))
