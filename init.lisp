(asdf:initialize-source-registry
 `(:source-registry
   (:tree ,(uiop:getcwd))
   :inherit-configuration))

(asdf:load-system "open-orders")
(open-orders.server.api:start)
(open-orders.client.main:main)
