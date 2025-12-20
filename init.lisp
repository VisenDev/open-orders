(declaim (optimize (speed 3) (safety 3) (debug 3)))

(unless (find-package "asdf")
  (load "tooling/asdf/build/asdf.lisp")
  )


(asdf:initialize-source-registry `(:source-registry (:tree ,(uiop:getcwd)) :ignore-inherited-configuration))
(asdf:load-system "clog")
(asdf:load-system "cl-db")
