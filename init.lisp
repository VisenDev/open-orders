(unless (find-package 'asdf)
  (format t ";;; Loading asdf...~%")
  (load "tooling/asdf/build/asdf.lisp")
  )

#+sbcl (declaim (sb-ext:muffle-conditions sb-ext:compiler-note))
#+sbcl (declaim (sb-ext:muffle-conditions cl:warning))

(asdf:initialize-source-registry `(:source-registry (:tree ,(uiop:getcwd)) :ignore-inherited-configuration))
(asdf:load-system "clog")

#+sbcl(declaim (sb-ext:unmuffle-conditions sb-ext:compiler-note))
#+sbcl(declaim (sb-ext:unmuffle-conditions cl:warning))

(asdf:load-system "cl-db")
