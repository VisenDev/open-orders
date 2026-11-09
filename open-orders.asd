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
  :serial t
  :components
  ((:module "modules/server"
     :components ((:file "db")
                  (:file "api")))))

(defsystem "open-orders/client" 
  :author "Robert Burnett"
  :depends-on ("open-orders/common"
               "drakma" "lparallel")
  :build-operation program-op
  :build-pathname "open-orders-client"
  :entry-point "open-orders.client.main::main"
  :serial t
  :components
  ((:module "modules/client"
    :components ((:file "main")))))
