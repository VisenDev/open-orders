(defpackage #:open-orders.client.main
  (:use #:cl #:open-orders.client.fetch)
  (:export
   #:main))
(in-package #:open-orders.client.main)

(defstruct (login-state (:conc-name l-))
  (username "" :type string)
  (password "" :type string)
  (loading-countdown 0 :type integer)
  (submitted-p nil :type boolean))

(defstruct (app-state (:conc-name a-))
  (auth-token nil))

(deftype page () '(member :login :app))

(defstruct (state (:conc-name s-))
  (active-page :login :type page)
  (login-state (make-login-state))
  (app-state (make-app-state)))

(defun parse (http-result-body)
  (let ((*read-eval* nil))
    (read-from-string http-result-body)))

(defun draw/login (state)
  (let ((l (s-login-state state)))
    (lui.qt:panel (lui:opt :width (lui:grow)
                           :height (lui:grow)
                           :padding (lui:units 1))
      (when (and (l-submitted-p l )
                 (<= (l-loading-countdown l) 0))
        (lui.qt:label "Wrong Username or Password"))

      (lui:panel (lui:opt :padding (lui:units 1))
        (lui:table (lui:opt :padding (lui:units 1))
          (lui:table-row ()
            (lui.qt:label "Username" (lui:opt :padding (lui:units 1)))
            (setf (l-username l)
                  (lui.qt:text-field :username (l-username l))))
          (lui:table-row ()
            (lui.qt:label "Password" (lui:opt :padding (lui:units 1)))
            (setf (l-password l) (lui.qt:text-field
                                  :password (l-password l)
                                  (lui:opt :password-p t)))))

        (when (lui.qt:button :login "login")
          (setf (l-loading-countdown l) 10)
          (setf (l-submitted-p l) t)
          (fetch "login"
                 (list (cons "username" (l-username l))
                       (cons "password" (l-password l)))
                 (lambda (result code)
                   (when (= code 200)
                     (setf (a-auth-token (s-app-state state)) (parse result))
                     (setf (s-active-page state) :app))))))

      (when (> (l-loading-countdown l) 0)
        (decf (l-loading-countdown l))
        (lui.qt:label "Loading...")))))

(defparameter *customer* #(:name :address
                           :primary-contact-first-name
                           :primary-contact-last-name
                           :primary-contact-email
                           :primary-contact-phone))

(defun instantiate (auth-token components)
  (fetch "create" (list (cons "auth-token" auth-token))
         (lambda (result code)
           (when (= code 200)
             (let* ((*read-eval* nil)
                    (id (read-from-string result)))
               (fetch "set-components"
                      (list (cons "auth-token" auth-token)
                            (cons "components-plist"
                                  (write-to-string
                                   (append (list :id id)
                                           (loop :for component :across components
                                                 :appending (list component ""))))))
                      (lambda (result code) (declare (ignore result code)))))))))

(let ((content ""))
  (defun draw/app (state)
    (let ((a (s-app-state state)))
      (lui.qt:panel (lui:opt :width (lui:grow)
                             :height (lui:grow)
                             :padding (lui:units 1))
        (lui.qt:label (a-auth-token a))
        (when (lui.qt:button :logout "Logout")
          (setf (s-login-state state) (make-login-state))
          (setf (s-app-state state) (make-app-state))
          (setf (s-active-page state) :login))

        (when (lui.qt:button :create-customer "Create Customer")
          (instantiate (a-auth-token a) *customer*))
        (when (lui.qt:button :refresh "Refresh")
          (fetch "query-components"
                 (list (cons "auth-token" (a-auth-token a))
                       (cons "components" "#()"))
                 (lambda (result code)
                   (declare (ignore code))
                   (setf content result))))
        (lui.qt:label content)))))

(let ((state (make-state)))
  (defun draw ()
    (ecase (s-active-page state)
      (:login (draw/login state))
      (:app (draw/app state))))

  (defun main ()
    (start)
    (setf state (make-state))
    (lui:with-palette (lui:+PALETTE-DEFAULT+)
      (lui:run #'draw))
    #-jscl
    (stop)
    (values)))
