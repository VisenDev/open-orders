(in-package #:asdf-user)

(defsystem "open-orders" 
  :author "Robert Burnett"
  :license "GPL-3.0"
  :depends-on ("trivial-features"
               "clog"
               "closer-mop"
               "defclass-std"
               "marshal"
               "sxql"
               "cl-dbi"
               "sqlite"
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
