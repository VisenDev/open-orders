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
               "uiop"
               "cl-pass"
               "lparallel"
               "trivial-types"
               "mito")
  :serial t
  :components ((:module "static-files"
                :components ((:static-file "pico.min.css")))
               (:module "src"
                :serial t
                :components ((:file "utils")
                             (:file "paths")
                             (:file "sql-table")
                             (:file "tables")
                             (:file "ui")
                             (:file "main")))))

(defsystem "open-orders/executable"
  :build-operation program-op
  :build-pathname "open-orders"
  :entry-point "open-orders.main:main"
  :depends-on ("open-orders")
  )
