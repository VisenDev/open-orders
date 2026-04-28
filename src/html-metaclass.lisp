(uiop:define-package #:open-orders.html-form
    (:use #:cl)
  (:import-from #:defclass-std
                #:defclass/std
                #:class/std)
  (:local-nicknames (#:a #:alexandria)
                    (#:mop #:closer-mop)
                    (#:db #:open-orders.db)))
(in-package #:open-orders.html-form)

(deftype input-type ()
  `(member :button
           :checkbox
           :color
           :date
           :datetime-local
           :email
           :file
           :hidden
           :image
           :month
           :number
           :password
           :radio
           :range
           :reset
           :search
           :submit
           :tel
           :text
           :time
           :url
           :week
           :foreign-key-edit))

(defclass html-form (db:sql-table) ())

(defclass/std html-form-effective-slot-definition (db::sql-table-effective-slot-definition)
  ((input-type name label)))

(defclass/std html-form-direct-slot-definition (db::sql-table-direct-slot-definition)
  ((input-type name label)))

(defmethod mop:validate-superclass ((class html-form) (superclass db:sql-table))
  t)


(defclass test-person ()
  ((age :accessor age :input-type :text))
  (:metaclass html-form))
