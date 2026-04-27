(defpackage #:open-orders.ui
  (:use #:cl)
  (:import-from #:defclass-std
                #:defclass/std
                #:class/std)
  (:local-nicknames (#:a #:alexandria)
                    (#:gui #:clog-gui)
                    (#:auth #:clog-auth)
                    (#:db #:open-orders.db)
                    (#:paths #:open-orders.paths)
                    (#:tbl #:open-orders.tables)))
(in-package #:open-orders.ui)

(defclass/std connection ()
  ((auth db menu)))

;; (defun make-token ()
;;   "Create a unique token used to associate a browser with a user"
;;   (crypto:byte-array-to-hex-string
;;    (crypto:random-data 16)))

(defun on-new-screen (body conn)
  (clog:destroy-children body)
  (clog:create-p body :content "Create new")
  )

(defun menu-bar-generate (body conn)
  (let* ((div (clog:create-div body))
         (new (clog:create-button div :content "New")))
    (clog:set-on-click new
                       (lambda (obj)
                         (declare (ignore obj))
                         (on-new-screen body conn)))))

(defun on-logged-in-screen (body conn)
  (clog:destroy-children body)
  (menu-bar-generate body conn)
  (clog:create-p body :content "Logged in :)"))

(defun on-login-screen (body conn)
  (let* ((div (clog:create-div body :style "padding:10px;"))
         (form (clog:create-form div))
         (lu  (clog:create-label form :content "Username: "))
         (user (clog:create-form-element form :string :label lu))
         (_ (clog:create-br form))
         (lp       (clog:create-label form :content "Password: "))
         (pass (clog:create-form-element form :password :label lp))
         (login (clog:create-button div :content "Login")))
    (declare (ignorable user pass _ login))
    (clog:set-on-click
     login
     (lambda (obj)
       (declare (ignorable obj))
       (let ((user-record
               (handler-case
                   (db:database-lookup (db conn) 'tbl:user (clog:value user))
                 (error (e) (clog:alert (clog:window body) e)))))
         (if user-record
             (if (cl-pass:check-password (clog:value pass) (tbl:hash user-record))
                 (on-logged-in-screen body conn)
                 (clog:alert (clog:window body) "Invalid Password"))
             (clog:alert (clog:window body) "Invalid Username")))))))

(defun on-new-window (body)
  (let ((conn (make-instance 'connection)))

    (setf (db conn) (tbl:database-connect))

    (clog:load-css (clog:html-document body)
                   "https://cdn.jsdelivr.net/npm/@picocss/pico@2.1.1/css/pico.min.css")

    (clog:set-html-on-close body "<script>close();</script>")
    (setf (clog:title (clog:html-document body)) "Overhead")
    (clog:enable-clog-popup)              ; To allow browser popups
    (clog:add-class body "w3-grey")

    (on-login-screen body conn)

    ;; Block until body has been closed
    (clog:run body)
    (tbl:database-disconnect (db conn)))
  
  )


(defun test ()
  (clog:initialize #'on-new-window)
  (clog:open-browser)
  )
