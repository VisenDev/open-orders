(cl:defpackage #:open-orders.utils
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria))
  (:export #:*let
           #:fn
           #:defpackage*))
(in-package #:open-orders.utils)

(eval-when (:compile-toplevel :load-toplevel :execute)
  
  (defun ignored-binding-p (binding)
    (and (listp binding)
         (char= #\_ (char (symbol-name (first binding)) 0))))
  (defun binding->name (binding)
    (if (listp binding)
        (first binding)
        binding)))

(defvar *package-cache* (make-hash-table))
(defmacro defpackage* (name &body clauses)
  "Defpackage except it also deletes to the package if it already exists
   to avoid package export errors"
  (a:when-let (existing (gethash name *package-cache*))
    (unless (equalp clauses existing)
     (delete-package name)))
  (setf (gethash name *package-cache*) clauses)
  `(defpackage ,name ,@clauses))

(defmacro *let (bindings &body body)
  "let* except it allows underscore prefixed vars to be ignored automatically"
  `(let* ,bindings
     (declare (ignorable ,@(remove-duplicates
                            (mapcar #'binding->name
                                    (remove-if-not #'ignored-binding-p bindings)))))
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
