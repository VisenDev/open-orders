(defpackage #:open-orders.pwa
  (:use #:cl))
(in-package #:open-orders.pwa)

(hunchentoot:define-easy-handler (manifest :uri "/manifest.json") ()
  (hunchentoot:handle-static-file
   (asdf:system-relative-pathname "open-orders"
                                  "src/pwa/manifest.json")
   "text/json"))


(hunchentoot:define-easy-handler (service-worker :uri "/service-worker.js") ()
  (hunchentoot:handle-static-file
   (asdf:system-relative-pathname "open-orders"
                                  "src/pwa/service-worker.js")
   "text/javascript"))

(hunchentoot:define-easy-handler (icon-512 :uri "/icon-512.png") ()
  (hunchentoot:handle-static-file
   (asdf:system-relative-pathname
    "open-orders" "src/pwa/icon-512.png")
   "image/png"))

(hunchentoot:define-easy-handler (icon-128 :uri "/icon-128.png") ()
  (hunchentoot:handle-static-file
   (asdf:system-relative-pathname
    "open-orders" "src/pwa/icon-128.png")
   "image/png"))

