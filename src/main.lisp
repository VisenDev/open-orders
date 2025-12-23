(defpackage #:cl-db/main
  (:use :cl :clog)
  (:export #:main))
(in-package #:cl-db/main)

(defmacro quick-defclass (name superclasses &body slots)
  (let* ((slot-forms nil))
    (loop :for slot :in slots
          :do (push
               `(,slot :initarg ,(intern (symbol-name slot) 'keyword) :accessor ,slot)
               slot-forms))
    `(defclass ,name ,superclasses ,slot-forms)
    )
  )

(defmacro callback (args &body body)
  `(lambda ,args (declare (ignorable ,@args))
     ,@body)
  )


(defun on-new-window (body)

  (load-css (html-document body) "https://www.w3schools.com/w3css/4/w3.css")
  (setf (title (html-document body)) "Open Orders")
  (let* ((menu (create-div body))
         (contents (create-div body :style "padding:10px;"))
         (home-tab (create-button menu :content "home"))
         (open-orders-tab (create-button menu :content "open-orders"))
         (clean-tab (create-button menu :content "clean"))
         )
    (set-on-click home-tab (callback (obj) (create-section contents :p :content "clicked home tab!")))
    (set-on-click open-orders-tab (callback (obj) (create-section contents :p :content "licked open-orders-tab")))
    (set-on-click clean-tab (callback (obj) (destroy-children contents)))
    ))

(defun main()
  (initialize #'on-new-window)
  (open-browser)
  )
