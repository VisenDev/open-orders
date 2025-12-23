(unless (find-package "asdf")
  (format t ";;; Loading asdf...~%")
  (load "tooling/asdf/build/asdf.lisp")
  )

(asdf:initialize-source-registry `(:source-registry (:tree ,(uiop:getcwd)) :ignore-inherited-configuration))
(asdf:load-system "clog")
(asdf:load-system "cl-db")
