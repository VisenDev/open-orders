(defsystem "cl-db" 
  :author "Robert Burnett"
  :license "Apache-2.0"
  :depends-on ("clog")
  :components ((:module "src"
                :components ((:file "main")))))

;;(defsystem "cl-db/executable"
;;  :build-operation program-op
;;  :build-pathname "cl-db"
;;  :entry-point "cl-db/main:main"
;;  :depends-on ("cl-db")
;;  )
