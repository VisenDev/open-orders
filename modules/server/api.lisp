(defpackage #:open-orders.server.api
  (:use #:cl #:open-orders.common.fn)
  (:local-nicknames (#:db #:open-orders.server.db))
  (:export
   #:main
   #:stop
   #:start))
(in-package #:open-orders.server.api)

(fn (parse t) ((http-result (or string null)))
  (when (stringp http-result)
    (let ((*read-eval* nil))
      (handler-case 
          (read-from-string http-result)
        (parse-error (e)
          (print e)
          nil)
        (end-of-file ()
          nil)))))

(hunchentoot:define-easy-handler (sync :uri "/sync") (id-version-pairs)
  (let ((ids/versions (parse id-version-pairs)))
    
    )
  )

(let (acceptor)
  (defun start ()
    (db:connect)
    (when acceptor
      (hunchentoot:stop acceptor))
    (setf acceptor (make-instance 'hunchentoot:easy-acceptor :port 8888))

    (hunchentoot:start acceptor))

  (defun stop ()
    (db:disconnect)
    (when acceptor
      (hunchentoot:stop acceptor))
    (setf acceptor nil))

  (defun main ()
    (start)
    (unwind-protect (loop (sleep 1))
      (stop))))
