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

(defun authentication-token-create ()
  "Create a unique token used to associate a browser with a user"
  (crypto:byte-array-to-hex-string
   (crypto:random-data 16)))

(defun on-new-screen (body conn)
  (clog:destroy-children body)
  (clog:create-p body :content "Create new"))

(defun menu-bar-generate (body conn)
  (let* ((div (clog:create-div body :class "container"))
         (header (clog:create-element div "nav"))
         (company-list (clog:create-unordered-list header))
         (company (clog:create-button (clog:create-list-item company-list)
                                 :content "American Campro"
                                 :class "outline"))
         (header-list (clog:create-unordered-list header))
         (new (clog:create-button (clog:create-list-item header-list) :content "New"
                                  :class "outline"))
         (open-orders (clog:create-button (clog:create-list-item header-list)
                                     :class "outline"
                                     :content "Open Orders"))
         (logout (clog:create-button (clog:create-list-item header-list)
                                     :class "outline"
                                     :content "Logout")))
    (declare (ignorable company))
    (clog:set-on-click new
                       (lambda (obj)
                         (declare (ignore obj))
                         (on-new-screen body conn)))
    (clog:set-on-click open-orders
                       (lambda (obj)
                         (declare (ignore obj))
                         (on-logged-in-screen body conn)))
    (clog:set-on-click logout
                       (lambda (obj)
                         (clog-auth:remove-authentication-token body)
                         (on-login-screen body conn)))
    (clog:set-on-click company
                       (lambda (obj)
                         (declare (ignorable obj))))))

(defun on-logged-in-screen (body conn)
  (clog:destroy-children body)
  (menu-bar-generate body conn)
  (clog:create-p body :content "Logged in :)"))

(defun on-login-screen (body conn)
  (clog:destroy-children body)
  (let* ((div (clog:create-div body :class "container"))
         (form (clog:create-form div :class "container"))
         (lu  (clog:create-label form :content "Username: "))
         (user (clog:create-form-element form :string :label lu))
         (_ (clog:create-br form))
         (lp       (clog:create-label form :content "Password: "))
         (pass (clog:create-form-element form :password :label lp))
         (stay-logged-in-label (clog:create-label
                                form
                                :content "Stay Logged In On This Device?"))
         (stay-logged-in (clog:create-form-element form :checkbox
                                                   :label stay-logged-in-label
                                                   :name "stay-logged-in"))
         (login (clog:create-button div :content "Login" :style "margin-top:20px;")))
    (declare (ignorable user pass _ login))

    (a:when-let (tok (clog-auth:get-authentication-token body))
      (a:when-let (found-user
                   (db:database-lookup (db conn) 'tbl:user tok
                                       :column 'tbl:authentication-token)))
      (on-logged-in-screen body conn))
    
    (labels ((try-login (obj)
               (declare (ignorable obj))
               (let ((user-record
                       (handler-case
                           (db:database-lookup (db conn) 'tbl:user (clog:value user))
                         (error (e) (clog:alert (clog:window body) e)))))
                 (if user-record
                     (if (cl-pass:check-password (clog:value pass) (tbl:hash user-record))
                         (progn
                           ;; auth token store/delete
                           (if (clog:checkedp stay-logged-in)
                               
                               (let ((tok (authentication-token-create)))
                                 (setf (tbl:authentication-token user-record) tok)
                                 (setf (tbl:authentication-token-timestamp user-record)
                                       (get-universal-time))
                                 (db:database-update (db conn) user-record)
                                 (clog-auth:store-authentication-token
                                  body tok))

                               ;; else remove auth token
                               (clog-auth:remove-authentication-token body))

                           ;; goto logged in screen
                           (on-logged-in-screen body conn))
                         
                         (clog:alert (clog:window body) "Incorrect Password"))
                     (clog:alert (clog:window body) "Invalid Username"))))
             (handle-keydown (obj event)
               (a:when-let (key (getf event :key))
                 (when (equal key "Enter")
                   (try-login obj)))))
      (clog:set-on-click login #'try-login)
      (clog:set-on-key-down pass #'handle-keydown)
      (clog:set-on-key-down user #'handle-keydown)
      (clog:set-on-key-down login #'handle-keydown)
      )))

(defun on-new-window (body)
  (let ((conn (make-instance 'connection)))

    (setf (db conn) (tbl:database-connect))

    (clog:load-css (clog:html-document body)
                   "https://cdn.jsdelivr.net/npm/@picocss/pico@2.1.1/css/pico.min.css")

    (clog:set-html-on-close body "<script>close();</script>")
    (setf (clog:title (clog:html-document body)) "Overhead")
    (clog:enable-clog-popup)              ; To allow browser popups

    (on-login-screen body conn)

    ;; Block until body has been closed
    (clog:run body)
    (tbl:database-disconnect (db conn)))
  
  )


(defun test ()
  (clog:initialize #'on-new-window)
  (clog:open-browser)
  )
