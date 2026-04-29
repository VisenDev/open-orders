(open-orders.utils:defpackage* #:open-orders.paths
  (:use #:cl)
  (:export #:*db-path*))
(in-package #:open-orders.paths)
(defparameter *db-path* "open-orders-database.sqlite3")
