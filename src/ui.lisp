(defpackage #:open-orders.ui
  (:use #:cl #:open-orders.utils)
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

(defun on-new-customer-screen (body conn)
  (clog:destroy-children body)
  (*let ((div (clog:create-div body :class "container"))
         (header (clog:create-element div "nav"))
         (header-list (clog:create-unordered-list header))
         (back (clog:create-button (clog:create-list-item header-list)
                                   :content "Back"
                                   :class "outline"))
         (form (clog:create-form div))
         (name (clog:create-form-element
                              form :text
                              :label (clog:create-label
                                      form
                                      :content "Customer Name")))
         (contact-first-name (clog:create-form-element
                              form :text
                              :label (clog:create-label
                                      form
                                      :content "Primary Contact First Name")))
         (contact-last-name (clog:create-form-element
                              form :text
                              :label (clog:create-label
                                      form
                                      :content "Primary Contact Last Name")))
         (contact-email (clog:create-form-element
                         form :text
                          :label (clog:create-label
                                  form
                                  :content "Primary Contact Email")))
         (contact-phone (clog:create-form-element
                          form :text
                          :label (clog:create-label
                                  form
                                  :content "Primary Contact Phone")))
         (_ (clog:create-br div))
         (_notes-label (clog:create-p div :content "Notes:"))
         (notes (clog:create-text-area div :rows 4 :name "Notes"))
         (save (clog:create-button div :content "Save")))

    (labels ((collect-person-data ()
               (let ((person (make-instance 'tbl:person)))
                 (setf (tbl:first-name person) (clog:value contact-first-name))
                 (setf (tbl:last-name person) (clog:value contact-last-name))
                 (setf (tbl:email person) (clog:value contact-email))
                 (setf (tbl:phone person) (clog:value contact-phone))

                 person)))
      (clog:set-on-click
       back
       (fn (obj)
         ;;; TODO maybe cache the fields so partially filled in data isn't lost
         ;;
         ;; (setf (clog:storage-element (clog:window body) :local "NewCustomerCache")
         ;;       (list (collect-person-data )))
         (on-logged-in-screen body conn)))
      (clog:set-on-click
       save
       (fn (obj)
         (let ((person (db:database-insert (db conn) (collect-person-data)))
               (customer (make-instance 'tbl:customer)))

           ;;; TODO figure out how to properly extract textarea value, as the below
           ;;; call currently returns null
           ;; (setf (db:notes customer) (clog:textarea-value notes "Notes"))
           (setf (tbl:primary-contact customer) person)
           (setf (tbl:name customer) (clog:value name))
           (db:database-insert (db conn) customer))
         (on-logged-in-screen body conn))))))

;; (defun on-new-screen (body conn)
;;   (clog:destroy-children body)
;;   (let* ((div (clog:create-div body :class "container"))
;;          (back (clog:create-button div :content "Back"))
;;          (new-customer (clog:create-button (clog:create-div div)
;;                                            :content "New Customer"))
;;          (new-material (clog:create-button (clog:create-div div)
;;                                            :content "New Material"))
;;          (new-order (clog:create-button (clog:create-div div)
;;                                         :content "New Purchase Order")))
;;     (clog:set-on-click back (lambda (obj)
;;                               (declare (ignore obj))
;;                               (on-logged-in-screen body conn)))
;;     (clog:set-on-click  new-customer (lambda (obj)
;;                                        (declare (ignore obj))
;;                                        (on-new-customer-screen body conn))))
;;   (clog:create-p body :content "Create new"))



(defun menu-bar-generate (body conn)
  (*let ((div (clog:create-div body :class "container"))
         (header (clog:create-element div "nav"))
         (header-list (clog:create-unordered-list header))
         (new-dropdown (clog:create-details (clog:create-list-item header-list)
                                            :class "dropdown"))
         (_summary (clog:create-summary new-dropdown :content "New"))
         (new-dropdown-list (clog:create-unordered-list new-dropdown))
         (new-customer-button (clog:create-button (clog:create-list-item new-dropdown-list )
                                                  :content "New Customer"
                                                  :class "outline"))
         (open-orders (clog:create-button (clog:create-list-item header-list)
                                          :class "outline"
                                          :content "Open Orders"))
         (logout (clog:create-button (clog:create-list-item header-list)
                                     :class "outline"
                                     :content "Logout")))
    (clog:set-on-click new-customer-button
                       (fn (obj) (on-new-customer-screen body conn)))
    (clog:set-on-click open-orders
                       (fn (obj) (on-logged-in-screen body conn)))
    (clog:set-on-click logout
                       (lambda (obj)
                         (declare (ignore obj))
                         (clog-auth:remove-authentication-token body)
                         (on-login-screen body conn)))))

(defun on-logged-in-screen (body conn)
  (clog:destroy-children body)
  (menu-bar-generate body conn)
  (clog:create-p body :content "Logged in :)"))

(defun on-login-screen (body conn)
  (clog:destroy-children body)
  (setf (clog:visiblep body) nil)
  (*let ((div (clog:create-div body :class "container"))
         (header (clog:create-element div "nav"))
         (_login-msg (clog:create-section (clog:create-list-item
                                           (clog:create-unordered-list header))
                                          :h1
                                          :content "Login"))
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
         (login (clog:create-button div :content "Login" :style "margin-top:20px;"))
         (msg (clog:create-p div :content "" :style "padding:10px;color:red;")))

    ;; If valid authentication token is found, go to logged in screen
    (a:when-let (tok (clog-auth:get-authentication-token body))
      (a:when-let (found-user
                   (db:database-lookup (db conn) 'tbl:user tok
                                       :column 'tbl:authentication-token)))
      (on-logged-in-screen body conn))

    ;; Reset msg on keyboard input
    (clog:set-on-key-down form (fn (obj event)
                                 (setf (clog:inner-html msg) "")))
    
    (labels ((try-login (obj)
               (declare (ignorable obj))
               (let ((user-record
                       (handler-case
                           (db:database-lookup (db conn) 'tbl:user (clog:value user))
                         (error (e) (clog:alert (clog:window body) e)))))
                 (cond
                   ((not user-record)
                    (setf (clog:inner-html msg) "Invalid Username"))
                   ((and user-record
                         (cl-pass:check-password
                          (clog:value pass) (tbl:hash user-record)))
                    (if (not (clog:checkedp stay-logged-in))
                        (clog-auth:remove-authentication-token body)

                        ;;else 
                        (let ((tok (authentication-token-create)))
                          (setf (tbl:authentication-token user-record) tok)
                          (setf (tbl:authentication-token-timestamp user-record)
                                (get-universal-time))
                          (db:database-update (db conn) user-record)
                          (clog-auth:store-authentication-token
                           body tok)))
                    
                    ;; goto logged in screen
                    (on-logged-in-screen body conn))
                   (t (setf (clog:content msg) "Incorrect Password")))))
             (handle-keydown (obj event)
               (a:when-let (key (getf event :key))
                 (when (equal key "Enter")
                   (try-login obj)))))
      (clog:set-on-click login #'try-login)
      (clog:set-on-key-down pass #'handle-keydown)
      (clog:set-on-key-down user #'handle-keydown)
      (clog:set-on-key-down login #'handle-keydown)

      ;; Finally render everything
      (setf (clog:visiblep body) t))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *pico-css*
    (format nil "<style>~a</style>"
            (uiop:read-file-string
             (asdf:system-relative-pathname
              "open-orders" "static-files/pico.min.css")))))

(defun on-new-window (body)
  
  (let ((conn (make-instance 'connection)))
    ;; (clog:load-css (clog:html-document body)
    ;;                "https://cdn.jsdelivr.net/npm/@picocss/pico@2.1.1/css/pico.min.css"
    ;;                )


    ;; Load css
    (clog:create-child (clog:head-element (clog:html-document body))
                       *pico-css*)


    (clog:set-html-on-close body "<script>close();</script>")
    (setf (clog:title (clog:html-document body)) "Overhead")
    (clog:enable-clog-popup)            ; To allow browser popups

    ;; loading bar
    (clog:create-child body "<div aria-busy=\"true\"/>")

    ;; Note: this can be slow so maybe this call should be delayed til later
    (setf (db conn) (tbl:database-connect))

    (on-login-screen body conn)

    ;; Block until body has been closed
    (clog:run body)
    (when (db conn)
      (tbl:database-disconnect (db conn)))))


(defun test ()
  (clog:initialize #'on-new-window)
  (clog:open-browser)
  )
