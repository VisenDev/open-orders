(defsystem "cl-db" 
  :author "Robert Burnett"
  :license "Apache-2.0"
  :depends-on ("clog")
  :components ((:module "src"
                :components ((:file "main")))))
