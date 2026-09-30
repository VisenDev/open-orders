(defpackage #:open-orders.documents
  (:use #:cl
        #:open-orders.tables)
  (:export
   #:generate-order-confirmation))
(in-package #:open-orders.documents)

(defparameter *bounds* pdf:*letter-portrait-page-bounds*)
(defparameter *layout-state* nil)

(defstruct (layout-state (:conc-name ls-))
  (margin 10 :type integer)
  (x 0 :type integer)
  (y 0 :type integer)
  (w (elt *bounds* 2) :type integer)
  (h (elt *bounds* 3) :type integer)
  ;; (font-size 16 :type integer)
  (font (pdf:get-font "Times-Roman")))

(defmacro with-pdf-layout (path &body body)
  `(let ((*layout-state* (make-layout-state)))
     (pdf:with-document ()
       (pdf:with-page (:bounds *bounds*)
         ,@body)
       (pdf:write-document ,path))))

(defun layout-text (text &key (font-size 16) (advance-line-p t))
  (pdf:in-text-mode
    (pdf:set-rgb-fill 0 0 0)
    (pdf:set-font (ls-font *layout-state*) font-size)
    (let ((dx (ls-x *layout-state*))
          (dy font-size;; (- (ls-y *layout-state*) font-size)
              ))
      )
    (pdf:move-text (ls-x *layout-state*) (- (ls-y *layout-state*) font-size))
    (pdf:draw-text text))
  (cond (advance-line-p
         (setf (ls-x *layout-state*) (ls-margin *layout-state*))
         (incf (ls-y *layout-state*) font-size))
        (t
         (incf (ls-x *layout-state*)
               (loop :for ch :across text
                     :sum (pdf:get-char-width ch (ls-font *layout-state*)
                                              font-size))))))

(defun layout-hr ()
  (pdf:set-rgb-stroke 0 0 0)
  (pdf:set-rgb-fill 0 0 0)
  (pdf:set-line-width 0.2)
  (pdf:move-to (ls-margin *layout-state*) (ls-y *layout-state*))
  (pdf:line-to (- (ls-w *layout-state*) (* 2 (ls-margin *layout-state*)))
               (ls-y *layout-state*))
  (pdf:close-fill-and-stroke)
  (incf (ls-y *layout-state*) 10))

(defun generate-order-confirmation (po-details &key
                                                 (path
                                                  "/tmp/confirmation.pdf"
                                                  ;; (format nil "/tmp/~a.pdf"
                                                  ;;  (open-orders.random:n-digit-number 10))
                                                  ))

  (with-pdf-layout path
    (layout-text "Order Confirmation" :font-size 32)
    (layout-text (universal-time->date-string (get-universal-time)))
    (layout-hr)

    )
  
  ;; (let ((w (elt pdf:*letter-portrait-page-bounds* 2))
  ;;       (h (elt pdf:*letter-portrait-page-bounds* 3))
  ;;       (h1 32)
  ;;       (h2 20)
  ;;       (p 16)
  ;;       (pad 5))
  ;;   (pdf:with-document ()
  ;;     (pdf:with-page (:bounds pdf:*letter-portrait-page-bounds*)
  ;;       (pdf:with-outline-level ("Order Confirmation" (pdf:register-page-reference))
  ;;         (let ((font (pdf:get-font "Times-Roman")))

  ;;           ;; Header
  ;;           (pdf:in-text-mode
  ;;             (pdf:set-font font h1)
  ;;             (pdf:move-text pad (- h h1))
  ;;             (pdf:draw-text "Order Confirmation")

  ;;             (pdf:set-font font h2)
  ;;             (pdf:move-text (* w 2/3) 0)
  ;;             (pdf:draw-text (universal-time->date-string
  ;;                             (get-universal-time))))

  ;;           ;; line
  ;;           (pdf:set-rgb-stroke 0 0 0)
  ;;   	    (pdf:set-rgb-fill 0.4 0.4 0.9)
  ;;   	    (pdf:set-line-width 0.2)
  ;;           (pdf:move-to 0 (- h h1 pad pad))
  ;;           (pdf:line-to w (- h h1 pad pad))
  ;;           (pdf:close-fill-and-stroke)

  ;;           ;; Order
  ;;           (pdf:in-text-mode
  ;;             (pdf:set-font font p)
  ;;             (pdf:set-rgb-fill 0 0 0)
  ;;             (pdf:move-text pad (- h h1 p 25))

  ;;             ;; Customer
  ;;             (pdf:draw-text "Sold To: ")
  ;;             (pdf:move-text 100 0)
  ;;             (pdf:draw-text (customer-name
  ;;                             (or 
  ;;                              (get-customer
  ;;                               ;; TODO remove the randomness here later
  ;;                               (or (po-details-customer-id po-details)
  ;;                                   (random 10)))
  ;;                              (make-customer))))

  ;;             ;; Po Number
  ;;             (pdf:move-text -100 (- (+ p pad)))
  ;;             (pdf:draw-text "P.O. Number:")
  ;;             (pdf:move-text 100 0)
  ;;             (pdf:draw-text (po-details-purchase-order po-details))

  ;;             ;; Bill Terms
  ;;             (pdf:move-text -100 (- (+ p pad)))
  ;;             (pdf:draw-text "Billing Terms:")
  ;;             (pdf:move-text 100 0)
  ;;             (pdf:draw-text (po-details-billing-terms po-details))
  ;;             )
            
  ;;           )))
  ;;     (pdf:write-document path)))
  path)

;; (open-orders.documents::generate-order-confirmation (make-po-details) )
