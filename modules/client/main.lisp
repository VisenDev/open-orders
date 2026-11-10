(defpackage #:open-orders.client.main
  (:use #:cl #:open-orders.client.fetch))
(in-package #:open-orders.client.main)

(let (auth-token
      (username "")
      (password "")
      processing-counter
      submitted-p)
  (defun draw ()
    (when (> submitted-p 0)
      (decf submitted-p))
    (lui.qt:panel (lui:opt :width (lui:grow)
                           :height (lui:grow))
      (when (and (null auth-token) submitted-p)
        (lui.qt:label "Wrong Username or Password"))
      (cond
        ((null auth-token)
         (setf username (lui.qt:text-field :username username))
         (setf password (lui.qt:text-field :password password
                                           (lui:opt :password-p t)))

         (when (lui.qt:button :login "login")
           (setf submitted-p t)
           (fetch "login"
                  (list (cons "username" username)
                        (cons "password" (url-rewrite:url-encode password)))
                  (lambda (http-result)
                    (setf submitted-p t)
                    (setf auth-token http-result))))

         (when submitted-p
           (lui.qt:label "Wrong Username or Password")))

        (t 
         
         (lui.qt:panel (lui:opt :width (lui:grow)
                                :height (lui:grow))
           (lui.qt:label auth-token)
           ;; (when (lui.qt:button :test "Incf Age?")
           ;;   (fetch (list (cons "id" )) )
           
           ;;   )
           ))))


    ))

(defun main ()
  (start)
  (lui:with-palette (lui:+PALETTE-LIGHT+)
    (lui:run #'draw))
  #-jscl
  (stop)
  (values))
