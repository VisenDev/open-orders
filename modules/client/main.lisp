(defpackage #:open-orders.client.main
  (:use #:cl #:open-orders.client.fetch)
  (:export
   #:main))
(in-package #:open-orders.client.main)

(defparameter *page-state* (make-hash-table :test 'equalp))
(defparameter *history* nil)
(defparameter *current-page* nil)

(defmacro defpage (name parameters static-variables &body body)
  (let ((hash-table-handle (gensym)))
    `(defun ,name (&rest parameters &key ,@parameters)
       (declare (ignorable ,@parameters))
       (unless (gethash (cons ',name parameters) *page-state*)
         (setf (gethash (cons ',name parameters) *page-state*) (make-hash-table)))
       
       (let ((,hash-table-handle (gethash (cons ',name parameters)
                                          *page-state*)))
         (declare (ignorable ,hash-table-handle))

         ,@(mapcan (lambda (form)
                   (when (listp form)
                     (list `(unless (gethash ',(first form) ,hash-table-handle)
                              (setf (gethash ',(first form) ,hash-table-handle)
                                    ,(second form))))))
                   static-variables)
         (symbol-macrolet
             ,(mapcar (lambda (var)
                        (if (listp var)
                            (list (first var)
                                  `(gethash ',(first var) ,hash-table-handle))
                            (list var `(gethash ',var ,hash-table-handle))))
               static-variables)
           ,@body)))))

(defun navigate (target &rest parameters)
  "Navigate to page, saving current page and its static variables"
  (when *current-page*
    (push *current-page* *history*))
  (setf *current-page* (cons target parameters)))

(defun redirect (target &rest parameters)
  "Navigate to page without saving current page in history or its static variables"
  (setf (gethash *current-page* *page-state*) nil)
  (setf *current-page* (cons target parameters)))

(defun back ()
  (when *history*
    (setf *current-page* (pop *history*))))

(defun draw-back-button ()
  (when *history*
    (when (lui.qt:button "Back")
      (back))))

(defpage login () ((status :waiting))
  (let (username
        password)
    (lui.qt:panel (lui:opt :width (lui:grow)
                           :height (lui:grow)
                           :padding (lui:units 1))
      (when (eq status :failed)
        (lui.qt:label "Incorrect username or password."))
      (lui:panel (lui:opt :padding (lui:units 1))
        (lui:table (lui:opt :padding (lui:units 1))
          (lui:table-row ()
            (lui.qt:label "Username" (lui:opt :padding (lui:units 1)))
            (setf username (lui.qt:text-field)))
          (lui:table-row ()
            (lui.qt:label "Password" (lui:opt :padding (lui:units 1)))
            (setf password (lui.qt:text-field (lui:opt :password-p t)))))

        (when (lui.qt:button "login")
          (setf status :loading)
          (fetch "login"
                 (list (cons "username" username)
                       (cons "password" password))
                 (lambda (result code)
                   (cond ((= code 200)
                          (redirect 'app-home :auth-token (parse result)))
                         (t (setf status :failed)))))))

      (when (eq status :loading)
        (lui.qt:label "Loading...")))))

(defpage app-home (auth-token) ()
  (lui.qt:panel (lui:opt :width (lui:grow)
                         :height (lui:grow))
    (lui.qt:panel (lui:opt :width (lui:grow)
                           :height (lui:grow)
                           :padding (lui:units 1))
      (lui:row ()
        (draw-back-button)
        (when (lui.qt:button "Customers")
          (navigate 'customers/list :auth-token auth-token))
        (when (lui.qt:button "Logout" )
          (redirect 'login)))
      (lui.qt:label "Hello from app"))))

(defpage customers/list (auth-token) ()
  (lui.qt:panel (lui:opt :width (lui:grow)
                         :height (lui:grow))
    (lui.qt:panel (lui:opt :width (lui:grow)
                           :height (lui:grow)
                           :padding (lui:units 1))
      (lui:row ()
        (draw-back-button)
        (when (lui.qt:button "Logout")
          (redirect 'login)))
      (lui.qt:label "Customers Page"))))

(defun parse (http-result-body)
  (let ((*read-eval* nil))
    (read-from-string http-result-body)))


(defparameter *customer* #(:name :address
                           :primary-contact-first-name
                           :primary-contact-last-name
                           :primary-contact-email
                           :primary-contact-phone))


(defun draw ()
  (execute-result-handlers)
  (apply (symbol-function (first *current-page*)) (rest *current-page*)))

(defun main ()
  (start)
  (redirect 'login)
  (lui:with-palette (lui:+PALETTE-DEFAULT+)
    (lui:run #'draw))
  #-jscl
  (stop)
  (values))



;; (defun instantiate (auth-token components)
;;   (fetch "create" (list (cons "auth-token" auth-token))
;;          (lambda (result code)
;;            (when (= code 200)
;;              (let* ((*read-eval* nil)
;;                     (id (read-from-string result)))
;;                (fetch "set-components"
;;                       (list (cons "auth-token" auth-token)
;;                             (cons "components-plist"
;;                                   (write-to-string
;;                                    (append (list :id id)
;;                                            (loop :for component :across components
;;                                                  :appending (list component ""))))))
;;                       (lambda (result code) (declare (ignore result code)))))))))

;; (let ((content ""))
;;   (defun draw/app ()
;;     (lui.qt:panel (lui:opt :width (lui:grow)
;;                            :height (lui:grow)
;;                            :padding (lui:units 1))
;;       (lui.qt:label (s-auth-token *s*))
;;       (when (lui.qt:button "Logout")
;;         (setf *s* (make-state)))

;;       (when (lui.qt:button "Create Customer")
;;         (instantiate (s-auth-token *s*) *customer*))
;;       (when (lui.qt:button "Refresh")
;;         (fetch "query-components"
;;                (list (cons "auth-token" (s-auth-token *s*))
;;                      (cons "components" "#()"))
;;                (lambda (result code)
;;                  (declare (ignore code))
;;                  (setf content result))))
;;       (lui.qt:label content))))
