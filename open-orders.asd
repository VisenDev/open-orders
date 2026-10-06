(in-package #:asdf-user)

(defsystem "open-orders" 
  :author "Robert Burnett"
  :description "Campro Open Orders Program"
  :depends-on ("uiop" "hunchentoot"
               "ironclad" "cl-pass" "asdf" "url-rewrite"
               "cl-pdf-parser" "cl-pdf" "cl-typesetting")
  :build-operation program-op
  :build-pathname "open-orders"
  :entry-point "open-orders.main:main"
  :serial t
  :components
  ((:module "src"
     :components ((:file "fn")
                  (:file "database")
                  (:file "html-generator")
                  (:file "pwa")
                  (:file "templates")
                  (:file "auth")
                  (:file "css")
                  (:file "derive-page")
                  (:file "random")
                  (:file "tables")
                  (:file "documents")
                  (:file "main")))))

