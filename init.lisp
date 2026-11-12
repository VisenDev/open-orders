(asdf:initialize-source-registry
 `(:source-registry
   (:tree ,(uiop:getcwd))
   :inherit-configuration))

(asdf:load-system "open-orders")
(handler-case (open-orders.server.api:start)
  (error (e)
    (format nil "~a" e)))
(open-orders.client.main:main)
