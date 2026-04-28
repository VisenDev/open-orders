(defpackage #:open-orders.utils
  (:use #:cl)
  (:export #:*let
           #:fn))
(in-package #:open-orders.utils)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun ignored-binding-p (binding)
    (and (listp binding)
         (char= #\_ (char (symbol-name (first binding)) 0)))))

(defmacro *let (bindings &body body)
  "let* except it allows underscore prefixed vars to be ignored automatically"
  `(let* ,bindings
     (declare (ignorable ,@(remove-duplicates
                            (remove-if-not #'ignored-binding-p bindings))))
     ,@body))

(defmacro fn (args &body body)
  "Shorter lambda that automatically makes args ignorable"
  `(lambda ,args
     (declare (ignorable ,@args))
     ,@body))

;; Local Variables:
;; eval: (font-lock-add-keywords
;;        'lisp-mode
;;        '(("(\\(fn\\|\\*let\\)\\_>"
;;           1 font-lock-keyword-face)))
;; End:
