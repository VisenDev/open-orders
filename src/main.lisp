(defpackage :cl-db/main
  (:use :cl :clog)
  (:export #:main))
(in-package :cl-db/main)

(defvar *state* 'home)

(defun home-tab-interactions (obj)
  (format t "pressed!~%")
  (setf (color obj) :red)
  )

(defun highlight (obj)
  (setf (color obj) "#d1d1d1"))
(defun normal (obj)
  (setf (color obj) "#ffffff"))

(defun apply-header-bar-styling (obj)
  (setf (display obj) :flex)
  (setf (align-items obj) :center)
  ;; (setf (justify-content obj) :space-between)
  (setf (height obj) "56px")
  (setf (background-color obj) "#ffffff")
  (setf (color obj) "#111111")
  (setf (style obj "box-shadow") "0 2px 8px rgba(0, 0, 0, 0.15)")
  (setf (style obj "padding") 0)
  (setf (style obj "margin") 0)
  (set-font obj "normal" "normal" 15 15 "arial"))

(defun on-new-window (body)
  (setf (title (html-document body)) "CL-DB")
  (setf (style body "margin") "0px")
  (setf (style body "padding") "0px")
  (setf (font-css body) "normal 16px arial")

  (let* ((menu (create-div body))
         (contents (create-div body))
         (home-tab (create-section menu :h2 :content "home"))
         (open-orders-tab (create-section menu :h2 :content "open-orders"))
         )

    (apply-header-bar-styling menu)
    (setf (style contents "padding") "8px")
    
    (setf (style home-tab "padding") 5)
    (setf (style open-orders-tab "padding") 5)
    
    ;;    (setf (border menu) :solid)
    ;; (set-on-pointer-enter home-tab #'highlight)
    ;; (set-on-pointer-leave home-tab #'normal)
    ;; (set-on-pointer-enter open-orders-tab #'highlight)
    ;; (set-on-pointer-leave open-orders-tab #'normal)

    (create-section contents :p :content "hello there")
    ))
  
(defun main()
  (initialize #'on-new-window)
  (open-browser))

(main)
