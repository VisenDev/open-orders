(uiop:define-package #:open-orders.client.main
  (:use #:cl 
        #:open-orders.client.cache
        #:open-orders.client.fetch
        #:open-orders.common.fn)
  (:export
   #:main))
(in-package #:open-orders.client.main)

(def (*page* symbol) 'login "Name of active draw function (page)")
(def (*page-args* list) nil "Args for the *page* function")
(def (*history* list) nil "List of (cons *page* *page-args*) for prior active pages")

(fn (page-reset (values)) ()
  (setf *page* 'login)
  (setf *page-args* nil)
  (setf *history* nil)
  (values))

(fn (page-redirect (values)) ((log-history-p boolean) (new-page symbol) &rest args)
  (when log-history-p
    (push (cons *page* *page-args*) *history* ))
  (setf *page* new-page)
  (setf *page-args* args)
  (values))

(fn (page-back (values)) ()
  (when *history*
    (let ((entry (pop *history*)))
      (setf *page* (car entry))
      (setf *page-args* (cdr entry))))
  (values))

(def (*auth-token* (or string null)) nil)
(def (*login-status* (member :waiting :loading :done :failed)) :waiting)

(fn (login-reset (values)) ()
  (setf *auth-token* nil)
  (setf *login-status* :waiting))

(defun login ()
  (let (username
        password)
    (lui:panel (lui:opt :width (lui:grow)
                        :height (lui:grow)
                        :padding (lui:units 1))
      (when (eq *login-status* :failed)
        (lui:label "Incorrect username or password."))
      (lui:panel (lui:opt :padding (lui:units 1))
        (lui:table (lui:opt :padding (lui:units 1))
          (lui:table-row ()
            (lui:label "Username" (lui:opt :padding (lui:units 1)))
            (setf username (lui:text-field)))
          (lui:table-row ()
            (lui:label "Password" (lui:opt :padding (lui:units 1)))
            (setf password (lui:text-field (lui:opt :password-p t)))))

        (when (lui:button "login")
          (setf *login-status* :loading)
          (with-fetch ("login" :username username
                               :password password)
              (result code)
            (cond ((= code 200)
                   (setf *login-status* :done)
                   (setf *auth-token* result)
                   (page-redirect nil 'home))
                  (t (setf *login-status* :failed))))))

      (when (eq *login-status* :loading)
        (lui:spinner)
        (lui:request-frame)))))

(fn draw-toplevel-controls ((selected (member :home :customers)))
  (lui:row (lui:opt :width (lui:grow))
      (when (lui:tab "Home" (lui:opt :selected-p (eq selected :home)))
        (page-redirect nil 'home))
      (when (lui:tab "Customers" (lui:opt :selected-p (eq selected :customers)))
        (page-redirect nil 'customers/list))
      (lui:spacer (lui:opt :width (lui:grow)))
      (when (lui:button "Logout")
        (page-reset)
        (login-reset))))

(defun home ()
  (lui:panel (lui:opt :width (lui:grow)
                      :height (lui:grow)
                      :padding (lui:units 1))
    
    (draw-toplevel-controls :home)
    ;; tab row

    ;; content
    (lui:label "Hello from app")))

(defun customers/edit (id)
  (lui:panel (lui:opt :width (lui:grow)
                      :height (lui:grow)
                      :padding (lui:units 1))
    (let ((customer-plist (cached-fetch "get-components"
                                        :auth-token *auth-token*
                                        :id (write-to-string id)))
          name)
      
      (cond ((null customer-plist)
             (lui:spinner))
            (t
             (lui:table ()
               (lui:table-row ()
                 (lui:table-cell () (lui:label "Name: "))
                 (setf name (lui:text-field)))
               (lui:table-row ()
                 (lui:table-cell ()
                   (when (lui:button "Save?")
                     (let ((components (copy-list customer-plist)))
                       (setf (getf components :name) name)
                       (fetch* "set-components"
                               :auth-token *auth-token*
                               :components (write-to-string components))))))))
            )
      )

    ))

(defun customers/list ()
  (lui:panel (lui:opt :width (lui:grow)
                      :height (lui:grow)
                      :padding (lui:units 1)
                      :gap (lui:units 1))
    (draw-toplevel-controls :customers)

    (lui:row ()
      (when (lui:button "New Customer")
        (with-fetch ("create" :auth-token *auth-token*)
            (id code)
          (assert (and id (= 200 code)))
          (fetch* "set" :auth-token *auth-token*
                        :id id :component ":customer-p" :value "t")
          (invalidate-cache))))

    (let ((customers (cached-fetch "query-components"
                                   :auth-token *auth-token*
                                   :components "#(:customer-p)")))
      (if customers
          (lui:panel (lui:opt :scrollbar-p t)
            (loop :for i :from 0
                  :for customer :across customers
                  :when (lui:button (write-to-string (getf customer :name))
                                    (lui:opt :width (lui:grow)
                                             :id i))
                    :do (page-redirect t 'customers/edit (getf customer :id))))
          (lui:spinner)))

    ;; (lui:label (format nil "~a" customers-plist))

    ;; (when loading-p
    ;;   (lui:request-frame)
    ;;   (lui:spinner))

    ;; (when (and (null customers-plist) (not loading-p))
    ;;   (setf loading-p t)
    ;;   (api-query-components
    ;;    auth-token #(:customer-p)
    ;;    (lambda (result http-code)
    ;;      (setf loading-p nil)
    ;;      (when (= 200 http-code)
    ;;        (setf customers-plist (or result "<error>"))))))
    ))

;; (defparameter *customer* #(:customer-p :name :address
;;                            :primary-contact-first-name
;;                            :primary-contact-last-name
;;                            :primary-contact-email
;;                            :primary-contact-phone))


(defun draw ()
  (execute-result-handlers)
  (apply (symbol-function *page*) *page-args*))

(defun main ()
  (start)
  (lui:with-palette (lui:+PALETTE-DEFAULT+)
    (lui:with-toolkit (:motif)
      (lui:run #'draw)))
  #-jscl
  (stop)
  (values))
