(defpackage #:open-orders.auth
  (:use #:cl
        #:open-orders.html-generator
        #:open-orders.templates
        #:open-orders.database)
  (:export
   #:perform-auth-check
   #:with-internal-page
   #:auth-token-create))
(in-package #:open-orders.auth)

(define-table user
    ((field (name password-hash auth-token)
            :type string :initform "")))

(declaim (ftype (function () string) auth-token-create))
(defun auth-token-create ()
  "Create a unique token used to associate a browser with a user"
  (crypto:byte-array-to-hex-string
   (crypto:random-data 16)))

(defparameter *auth-cookie* "AUTH_TOKEN")
(defun perform-auth-check ()
  (let ((token (hunchentoot:cookie-in *auth-cookie*)))
    (unless token
      (hunchentoot:redirect "/login"))
    (let ((user (find token (get-every-user) :test #'string= :key #'user-auth-token)))
      (unless user
        (hunchentoot:redirect "/login")))))

(hunchentoot:define-easy-handler (logout :uri "/logout") ()
  (hunchentoot:set-cookie *auth-cookie* :value nil)
  (hunchentoot:redirect "/login"))

(hunchentoot:define-easy-handler (login :uri "/login") ((username :init-form nil)
                                                        (password :init-form nil))

  (let ((errmsg nil))

    ;; Attempt login
    (when (or username password)
      (let ((user (find username (get-every-user)
                        :test #'string= :key #'user-name)))
        (cond
          ;; Success
          ((and user (cl-pass:check-password password (user-password-hash user)))
           (setf (user-auth-token user) (auth-token-create))
           (set-user user)
           (hunchentoot:set-cookie *auth-cookie*
                                   :value (user-auth-token user))
           (hunchentoot:redirect "/"))

          ;; User found but wrong password
          (user
           (setf errmsg "Incorrect Password"))

          ;; Incorrect username and password
          (t
           (setf errmsg "Unknown Username")))))

    ;; Display login screen
    (setf (hunchentoot:content-type*) "text/html")
    (with-page
      (h1 () "Campro Login")
      (when errmsg
        (p () errmsg))
      (form (:method "POST" :action "/login")
        (html-table ()
          (tr ()
            (td () (label (:for "username") "Username"))
            (td () (input (:type "text" :name "username"
                           :id "username" :autocomplete "on"))))
          (tr ()
            (td () (label (:for "password") "Password"))
            (td () (input (:type "password" :name "password"
                           :id "password" :autocomplete "on"))))
          (tr ()
            (td () (input (:type "submit")))))))))

(defmacro with-internal-page (&body body)
  `(progn
     (perform-auth-check)
     (with-page
       (h1 () "Campro Open Orders")
       ,@body)))

(defun user-create-new (name password)
  (set-user (make-user
             :name name
             :password-hash (cl-pass:hash password))))
