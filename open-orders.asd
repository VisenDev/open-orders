(in-package #:asdf-user)

(defsystem "open-orders" 
  :author "Robert Burnett"
  :description "Campro Open Orders Program"
  :depends-on ("uiop" "hunchentoot"
                      "ironclad" "cl-pass" "asdf" "url-rewrite")
  :build-operation program-op
  :build-pathname "open-orders"
  :entry-point "open-orders.main:main"
  :serial t
  :components
  ((:module "src"
     :components ((:file "fn")
                  (:file "database")
                  (:file "html-generator")
                  (:file "templates")
                  (:file "auth")
                  (:file "main")))))

