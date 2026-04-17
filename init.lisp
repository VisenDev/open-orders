(let ((*standard-output* (make-broadcast-stream)))
  #+sbcl (declaim (sb-ext:muffle-conditions sb-ext:compiler-note))
  #+sbcl (declaim (sb-ext:muffle-conditions cl:warning))

  (asdf:initialize-source-registry `(:source-registry (:tree ,(uiop:getcwd)) :ignore-inherited-configuration))
  (asdf:load-system "clog")

  #+sbcl(declaim (sb-ext:unmuffle-conditions sb-ext:compiler-note))
  #+sbcl(declaim (sb-ext:unmuffle-conditions cl:warning)))

(asdf:load-system "open-orders")
