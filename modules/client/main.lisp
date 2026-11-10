(defpackage #:open-orders.client.main
  (:use #:cl #:open-orders.client.fetch)
  (:export
   #:main))
(in-package #:open-orders.client.main)

(let (auth-token
      (username "")
      (password "")
      (processing-counter 0)
      submitted-p)
  (defun draw ()
    (when (> processing-counter 0)
      (decf processing-counter))
    (lui.qt:panel (lui:opt :width (lui:grow)
                           :height (lui:grow)
                           :padding (lui:units 1))
      (when (and (null auth-token) submitted-p (<= processing-counter 0))
        (lui.qt:label "Wrong Username or Password"))
      (cond
        ((null auth-token)
         (lui:panel (lui:opt :padding (lui:units 1))
           (lui:table (lui:opt :padding (lui:units 1))
             (lui:table-row ()
               (lui.qt:label "Username" (lui:opt :padding (lui:units 1)))
               (setf username (lui.qt:text-field :username username)))
             (lui:table-row ()
               (lui.qt:label "Password" (lui:opt :padding (lui:units 1)))
               (setf password (lui.qt:text-field :password password
                                                 (lui:opt :password-p t)))))

           (when (lui.qt:button :login "login")
             (setf processing-counter 10)
             (setf submitted-p t)
             (fetch "login"
                    (list (cons "username" username)
                          (cons "password" (url-rewrite:url-encode password)))
                    (lambda (http-result)
                      (setf submitted-p t)
                      (setf auth-token http-result)))))

             (when (> processing-counter 0)
               (lui.qt:label "Loading...")))

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
