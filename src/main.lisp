(unless (find-package :clog) (asdf:load-system :clog))
(defpackage :cl-db/main
  (:use :cl :clog)
  (:export #:main))
(in-package :cl-db/main)

(defparameter *fg*        "#e4e4ef")
(defparameter *fg+1*      "#f4f4ff")
(defparameter *fg+2*      "#f5f5f5")
(defparameter *white*     "#ffffff")
(defparameter *black*     "#000000")
(defparameter *bg-1*      "#101010")
(defparameter *bg*        "#181818")
(defparameter *bg+1*      "#282828")
(defparameter *bg+2*      "#453d41")
(defparameter *bg+3*      "#484848")
(defparameter *bg+4*      "#52494e")
(defparameter *red-1*     "#c73c3f")
(defparameter *red*       "#f43841")
(defparameter *red+1*     "#ff4f58")
(defparameter *green*     "#73c936")
(defparameter *yellow*    "#ffdd33")
(defparameter *brown*     "#cc8c3c")
(defparameter *quartz*    "#95a99f")
(defparameter *niagara-2* "#303540")
(defparameter *niagara-1* "#565f73")
(defparameter *niagara*   "#96a6c8")
(defparameter *wisteria*  "#9e95c7")

(defmacro quick-defclass (name superclasses &body slots)
  (let* ((slot-forms nil))
    (loop :for slot :in slots
          :do (push
               `(,slot :initarg ,(intern (symbol-name slot) 'keyword) :accessor ,slot)
               slot-forms))
    `(defclass ,name ,superclasses ,slot-forms)
    )
  )

(quick-defclass theme ()
  fg
  bg
  clickable-normal
  clickable-hover
  header-fg
  header-bg
  font
  corners)

(defparameter *theme-gruber-darker*
  (make-instance 'theme
                 :fg "#e4e4ef"
                 :bg "#181818"
                 :clickable-normal "#ffdd33"
                 :clickable-hover "#ffffff"
                 :header-bg "#282828"
                 :header-fg "#f4f4ff"
                 :font "normal 16px monospace"
                 :corners "10px"))

(defclass app ()
  ((menu-tab :initform :home :accessor menu-tab)
   (database :accessor database :initarg :database)
   (theme :initarg :theme :initform *theme-gruber-darker* :accessor theme)))

(defun apply-header-bar-styling (app obj)
  (setf (display obj) :flex)
  (setf (align-items obj) :center)
  ;; (setf (justify-content obj) :space-between)
  (setf (height obj) "56px")
  (setf (background-color obj) (header-bg (theme app)))
  (setf (color obj) (header-fg (theme app)))
  (setf (style obj "box-shadow") "0 2px 8px rgba(0, 0, 0, 0.15)")
  (setf (style obj "padding") 0)
  (setf (style obj "margin") 0)
  (setf (style obj "border-radius") "10px")
  (setf (style obj "padding-left") "10px"))

(defun apply-clickable-styling (app obj)
  (setf (cursor obj) :pointer)
  (setf (color obj) (clickable-normal (theme app)))
  (set-on-pointer-enter obj (lambda (obj) (setf (color obj) (clickable-hover (theme app)))))
  (set-on-pointer-leave obj (lambda (obj) (setf (color obj) (clickable-normal (theme app)))))
  (setf (style obj "padding") "5px")
  )

;;(defun database-create-tables (body db &optional second-try &aux stmt)
;;  (handler-case
;;      (progn
;;        (setf stmt (dbi:prepare db "create table customers(id integer primary key autoincrement);"))
;;        (dbi:execute stmt)
;;        )
;;    (sqlite-error ()
;;      (progn (clog:execute ))
;;      (setf stmt (dbi:prepare db "drop table customers;"))
;;      (dbi:execute stmt)
;;      ))
;;  )


(defun open-orders-page (body &aux my/app)
  (setf my/app (make-instance 'app))
  
  (setf (title (html-document body)) "CL-DB")
  (setf (style body "margin") "0px")
  (setf (style body "padding") "0px")
  (setf (font-css body) (font (theme my/app)))
  (setf (background-color body) (bg (theme my/app)))

  (let* ((menu (create-div body))
         (contents (create-div body :style "padding:10px;"))
         (home-tab (create-section menu :h2 :content "home"))
         (open-orders-tab (create-section menu :h2 :content "open-orders"))
         )
    (setf (color contents) (fg (theme my/app)))
    (apply-header-bar-styling my/app menu)
    
    (apply-clickable-styling my/app (clog:create-a body :content "Click Me!" :link "/page2" ))
    
    (set-on-click home-tab (lambda (obj) (setf (menu-tab my/app) (html-id obj))))
    (set-on-click open-orders-tab (lambda (obj) (setf (menu-tab my/app) (html-id obj))))
    
    (apply-clickable-styling my/app home-tab)
    (apply-clickable-styling my/app open-orders-tab)

    (link-slot-to-element my/app menu-tab contents)

    ;;Refresh menu tab 
    (setf (menu-tab my/app) :foo)
    ))

(defun page2 (body)
  (create-section body :h1 :content "Hello from page2")
  )

(defun main()
  (initialize #'on-new-window)
  (set-on-new-window 'page2 :path "/page2")
  (open-browser)
  )
