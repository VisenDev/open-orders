(defpackage #:open-orders.documents
  (:use #:cl
        #:open-orders.tables)
  (:export
   #:generate-order-confirmation))
(in-package #:open-orders.documents)

(defun generate-order-confirmation (po-details &key
                                    (path
                                     "/tmp/confirmation.pdf"
                                     ;; (format nil "/tmp/~a.pdf"
                                          ;;  (open-orders.random:n-digit-number 10))
                                          ))
  (let ((w (elt pdf:*letter-portrait-page-bounds* 2))
        (h (elt pdf:*letter-portrait-page-bounds* 3))
        (h1 32)
        (h2 20)
        (p 16)
        (pad 5))
    (pdf:with-document ()
      (pdf:with-page (:bounds pdf:*letter-portrait-page-bounds*)
        (pdf:with-outline-level ("Order Confirmation" (pdf:register-page-reference))
          (let ((font (pdf:get-font "Times-Roman")))

            ;; Header
	        (pdf:in-text-mode
	          (pdf:set-font font h1)
	          (pdf:move-text pad (- h h1))
              (pdf:draw-text "Order Confirmation")

              (pdf:set-font font h2)
	          (pdf:move-text (* w 2/3) 0)
              (pdf:draw-text (universal-time->date-string
                              (get-universal-time))))

            ;; line
            (pdf:set-rgb-stroke 0 0 0)
		    (pdf:set-rgb-fill 0.4 0.4 0.9)
		    (pdf:set-line-width 0.2)
	        (pdf:move-to 0 (- h h1 pad pad))
            (pdf:line-to w (- h h1 pad pad))
            (pdf:close-fill-and-stroke)

            ;; Order
            (pdf:in-text-mode
              (pdf:set-font font p)
              (pdf:set-rgb-fill 0 0 0)
              (pdf:move-text pad (- h h1 p 25))

              ;; Customer
              (pdf:draw-text "Sold To: ")
              (pdf:move-text 100 0)
              (pdf:draw-text (customer-name
                              (or 
                               (get-customer
                                ;; TODO remove the randomness here later
                                (or (po-details-customer-id po-details)
                                    (random 10)))
                               (make-customer))))

              ;; Po Number
              (pdf:move-text -100 (- (+ p pad)))
              (pdf:draw-text "P.O. Number:")
              (pdf:move-text 100 0)
              (pdf:draw-text (po-details-purchase-order po-details))

              ;; Bill Terms
              (pdf:move-text -100 (- (+ p pad)))
              (pdf:draw-text "Billing Terms:")
              (pdf:move-text 100 0)
              (pdf:draw-text (po-details-billing-terms po-details))
              )
            
            )))
      (pdf:write-document path)))
  path)

;; (open-orders.documents::generate-order-confirmation (make-po-details) )
