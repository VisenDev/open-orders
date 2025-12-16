(defpackage :db/main
  (:use :clim-lisp :clim :cl-dbi)
  (:export :main)
  )
(in-package :db/main)

;; Definition of the structure of a minimum app
(define-application-frame db-app ()
  ((connection :initform (connect :sqlite
                                   :database-name ".main.sqlite3"))
  )

  (:panes
    (app :application
         :display-time nil
         :height 400
         :width 600)
    (my-interactor :interactor
                   :height 400
                   :width 20c0))
  (:layouts
    (default (horizontally () app my-interactor))))

(define-db-app-command (com-quit :name t) ()
  (frame-exit *application-frame*))


(defun main ()
  (run-frame-top-level (make-application-frame 'db-app)))


