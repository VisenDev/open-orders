(in-package #:asdf-user)

(defsystem "open-orders" 
  :author "Robert Burnett"
  :license "Apache-2.0"
  :depends-on ("trivial-features"
               "clog"
               "closer-mop"
               "defclass-std"
               "marshal"
               "uiop")
  :serial t
  :components ((:module "src"
                :components ((:file "database")
                             (:file "main")))))

(defsystem "open-orders/executable"
  :build-operation program-op
  :build-pathname "open-orders"
  :entry-point "open-orders.main:main"
  :depends-on ("open-orders")
  )
