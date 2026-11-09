(defpackage #:open-orders.client.main
  (:use #:cl))
(in-package #:open-orders.client.main)

(defun main ()
  (setf lparallel:*kernel* (lparallel:make-kernel 2))
  (let ((content "")
        (channel (lparallel:make-channel)))
    
    (lui:with-palette (lui:+PALETTE-LIGHT+)
      (lui:run
       (lambda ()
         (lui.qt:panel (lui:opt :width (lui:grow)
                                :height (lui:grow))
           (when (lui.qt:button :test "Incf Age?")
             (lparallel:submit-task
              channel
              (lambda ()
                (drakma:http-request
                 (format nil "http://localhost:8888/set?id=1&component=age&value=~a"
                         (1+ (read-from-string
                              (drakma:http-request
                               "http://localhost:8888/get?id=1&component=age")))))))
             (setf content (drakma:http-request
                            "http://localhost:8888/get?id=1&component=age" )))
           (lui.qt:label content)))
       :backend :x11))
    (lparallel:end-kernel :wait t)
    )
  
  (values))
