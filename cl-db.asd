(in-package #:asdf-user)

(defsystem "cl-db" 
  :author "Robert Burnett"
  :license "Apache-2.0"
  :depends-on ("clog"
               "closer-mop"
               "defclass-std"
               "marshal"
               "uiop")
  :serial t
  :components ((:module "src"
                :components ((:file "database")
                             (:file "main")))))

;;(defsystem "cl-db/executable"
;;  :build-operation program-op
;;  :build-pathname "cl-db"
;;  :entry-point "cl-db/main:main"
;;  :depends-on ("cl-db")
;;  )
