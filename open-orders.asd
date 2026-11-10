(in-package #:asdf-user)

(defsystem "open-orders/common" 
  :author "Robert Burnett"
  :depends-on ()
  :serial t
  :components
  ((:module "modules/common"
     :components ((:file "fn")))))

(defsystem "open-orders/server" 
  :author "Robert Burnett"
  :depends-on ("open-orders/common"
               "uiop" "cl-pass" "sqlite"
               "hunchentoot" "trivial-types")
  :build-operation program-op
  :build-pathname "open-orders-server"
  :entry-point "open-orders.server.api:main"
  :serial t
  :components
  ((:module "modules/server"
     :components ((:file "db")
                  (:file "api")))))

(defsystem "open-orders/client" 
  :author "Robert Burnett"
  :depends-on ("open-orders/common"
               "drakma" "lparallel"
               "lui" "url-rewrite")
  :build-operation program-op
  :build-pathname "open-orders-client"
  :entry-point "open-orders.client.main:main"
  :serial t
  :components
  ((:module "modules/client"
    :components ((:file "fetch")
                 (:file "main")))))

(defsystem "open-orders"
  :author "Robert Burnett"
  :description "Business Order Management System"
  :depends-on ("open-orders/client" "open-orders/server"))
