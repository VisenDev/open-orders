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
    (when (lui:button "Back")
      (back))))

(defpage login () ((status :waiting))
  (let (username
        password)
    (lui:panel (lui:opt :width (lui:grow)
                           :height (lui:grow)
                           :padding (lui:units 1))
      (when (eq status :failed)
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
          (setf status :loading)
          (fetch "login"
                 (list (cons "username" username)
                       (cons "password" password))
                 (lambda (result code)
                   (cond ((= code 200)
                          (redirect 'app-home :auth-token (parse result)))
                         (t (setf status :failed)))))))

      (when (eq status :loading)
        (lui:label "Loading...")))))

(defpage app-home (auth-token) ()
  (lui:panel (lui:opt :width (lui:grow)
                         :height (lui:grow))
    (lui:panel (lui:opt :width (lui:grow)
                           :height (lui:grow)
                           :padding (lui:units 1))
      (lui:row ()
        (draw-back-button)
        (when (lui:button "Customers")
          (navigate 'customers/list :auth-token auth-token))
        (when (lui:button "Logout" )
          (redirect 'login)))
      (lui:label "Hello from app"))))

(defpage customers/list (auth-token) ()
  (lui:panel (lui:opt :width (lui:grow)
                         :height (lui:grow))
    (lui:panel (lui:opt :width (lui:grow)
                           :height (lui:grow)
                           :padding (lui:units 1))
      (lui:row ()
        (draw-back-button)
        (when (lui:button "Logout")
          (redirect 'login)))
      (lui:label "Customers Page"))))

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
    (lui:with-toolkit (:motif)
      (lui:run #'draw)))
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
;;     (lui:panel (lui:opt :width (lui:grow)
;;                            :height (lui:grow)
;;                            :padding (lui:units 1))
;;       (lui:label (s-auth-token *s*))
;;       (when (lui:button "Logout")
;;         (setf *s* (make-state)))

;;       (when (lui:button "Create Customer")
;;         (instantiate (s-auth-token *s*) *customer*))
;;       (when (lui:button "Refresh")
;;         (fetch "query-components"
;;                (list (cons "auth-token" (s-auth-token *s*))
;;                      (cons "components" "#()"))
;;                (lambda (result code)
;;                  (declare (ignore code))
;;                  (setf content result))))
;;       (lui:label content))))
