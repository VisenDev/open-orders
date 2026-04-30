(open-orders.utils:defpackage* #:open-orders.object-form
  (:use #:cl)
  (:import-from #:defclass-std
                #:defclass/std
                #:class/std)
  (:import-from #:open-orders.utils
                #:fn
                #:*let)
  (:local-nicknames (#:a #:alexandria)
                    (#:auth #:clog-auth)
                    (#:sql #:open-orders.sql-table)
                    (#:paths #:open-orders.paths)
                    (#:tbl #:open-orders.tables)
                    (#:mop #:closer-mop)))
(in-package #:open-orders.object-form)

(declaim (optimize (debug 3) (safety 3)))

;; (deftype element-type ()
;;   `(member :button
;;            :checkbox
;;            :color
;;            :date
;;            :datetime-local
;;            :email
;;            :file
;;            :hidden
;;            :image
;;            :month
;;            :number
;;            :password
;;            :radio
;;            :range
;;            :reset
;;            :search
;;            :submit
;;            :tel
;;            :text
;;            :time
;;            :url
;;            :week))

(declaim (ftype (function (t) string) to-string))
(defun to-string (object)
  (format nil "~a" object))

(defstruct (config (:conc-name config.))
  (label nil :type (or string null))
  (display-value-in-label-p nil :type boolean)
  (value nil :type t)
  (type nil :type (or clog:form-element-type null))
  (radio-choices nil :type list) ;;only valid for radio input type
  (min nil :type (or integer null))
  (max nil :type (or integer null))
  (placeholder "" :type string)
  (class "" :type string)
  (pattern nil :type (or string null))
  (lisp->html #'to-string)
  (html->lisp #'identity))

(defun getf* (plist symbol &optional default)
  (getf plist symbol
        (getf plist (a:make-keyword symbol)
              default)))

(defmethod lisp-type->form-element-type ((instance standard-object) type)
)

(defun config-finalize (config slotd)

  ;; Add label if missing
  (unless (config.label config)
    (setf (config.label config) (to-string
                                 (mop:slot-definition-name slotd))))

  ;; handle element type
  (unless (config.type config)
    (let ((type (mop:slot-definition-type slotd)))
      (cond
        ;; checkbox
        ((subtypep type 'boolean)
         (setf (config.type config) :checkbox))

        ;; radio
        ((and 
          (listp type)
          (eq 'member (first type)))
         (setf (config.type config) :radio)
         (when (null (config.radio-choices config))
           (setf (config.radio-choices config) (rest type))))

        ;; file-author
        ((subtypep type 'pathname)
         (setf (config.type config) :file)
         (setf (config.html->lisp config) #'parse-namestring))

        ;; integer
        ((subtypep type 'integer)
         (if (and (config.min config)
                    (config.max config))
           (setf (config.type config) :range)
           ;;else
           (progn
             (setf (config.type config) :text)
             (unless (config.pattern config)
               (setf (config.pattern config) "/[0-9]*/")))))

        ;; number
        ((subtypep type 'real)
         (setf (config.type config) :text)
         (unless (config.pattern config)
           (setf (config.pattern config) "/[0-9.]*/")))

        (t
         (setf (config.type config) :text))))))

(defun class-ui* (slot-config-plist clog-obj instance &key (html-class ""))
  (let ((form (clog:create-form clog-obj :class html-class)))
    (dolist (slotd (mop:class-slots (class-of instance)))
      (let* ((name (mop:slot-definition-name slotd))
             (config (getf* slot-config-plist name
                            (make-config))))
        (config-finalize config slotd)

        (let ((label (clog:create-label
                      form :content (config.label config))))

          (case (config.type config)
            ((:radio)
             (let ((name (to-string (gensym "Radio"))))
               (dolist (choice (config.radio-choices config))
                 (clog:create-form-element
                  :radio form :name name :label label
                  :content (to-string choice))))
             )
            (otherwise
             (let ((args
                     (list form (config.type config)
                           :placeholder (config.placeholder config))))
               (a:when-let (min (config.min config))
                 (a:appendf args (list :min min)))
               (a:when-let (max (config.max config))
                 (a:appendf args (list :max max)))
               (a:when-let (class (config.class config))
                 (a:appendf args (list :class class)))
               (a:when-let (value (and (slot-boundp instance name)
                                       (slot-value instance name)))
                 (a:appendf
                  args (list :value (funcall
                                     (config.lisp->html config) value))))

               (apply #'clog:create-form-element args)))))))))

(defmacro class-ui (slot-config-plist clog-obj instance &key (html-class ""))
  `(class-ui*
    (list ,@(map-plist-values (lambda (args)
                              (cons 'make-config args))
                            slot-config-plist))
    ,clog-obj ,instance :html-class ,html-class))

(defun test-class-ui (body)
  (let ((p (make-instance 'person)))
    (class-ui* (list :email (make-config :type :email))
               body p
               :html-class "container")
    )
  )



(defun create-form-from-object (clog-obj instance slot-config-plist &key (form-class ""))

  "Old version"
  (loop
    :with form = (clog:create-form clog-obj :class form-class)
    :for slot :in (mop:class-slots (class-of instance))
    :for name = (mop:slot-definition-name slot)
    :for config = (or (getf slot-config-plist name)
                      (getf slot-config-plist (a:make-keyword name))
                      (make-config))
    :for div = (clog:create-div form)
    :for label = (clog:create-label div :content (or (config.label config) name))
    :for args = (list div (config.type config)
                      :placeholder (config.placeholder config))
    :do (a:when-let (min (config.min config))
          (a:appendf args (list :min min)))
        (a:when-let (max (config.max config))
          (a:appendf args (list :max max)))
        (a:when-let (class (config.class config))
          (a:appendf args (list :class class)))
        (a:when-let (value (and (slot-boundp instance name)
                                (slot-value instance name)))
          (a:appendf
           args (list :value (funcall
                              (config.lisp-to-element config) value))))

        (let ((form-element (apply #'clog:create-form-element args)))
          (clog:label-for label form-element)
          (let ((form-element form-element)
                (instance instance)
                (config config))
            (clog:set-on-change
             form-element
             (lambda (obj)
               (declare (ignore obj))
               (if (funcall
                    (config.validate-function config)
                    (clog:value form-element))
                   (progn (print "invalid")
                          (setf (clog:color form-element) :red)) ;;; TODO, find a better way to show validity
                   (setf (clog:color form-element) :black))
               (setf (slot-value instance name)
                     (funcall (config.element-to-lisp config)
                              (clog:value form-element)))))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun map-plist-values (function plist)
    (loop :for i :from 0
          :for item :in plist
          :for value-p = (oddp i)
          :for result = (if value-p
                            (funcall function item)
                            item)
          :collect result)))

(defmacro create-form-from-object* (clog-obj instance slot-config-plist)
  `(create-form-from-object
    ,clog-obj ,instance
    (list ,@(map-plist-values (lambda (args)
                              (cons 'make-config args))
                            slot-config-plist))))



;;;; TEST

(class/std person age name hobbies siblings phone email address notes)

(defun test ()

  (clog:initialize
   (lambda (body)

     (clog:load-css
      (clog:html-document body)
      "https://cdn.jsdelivr.net/npm/@picocss/pico@2.1.1/css/pico.min.css")

     (test-class-ui body)
     
     ;; (let ((person (make-instance 'person :name "Bobby")))
     ;;   (create-form-from-object
     ;;    body person
     ;;    (list :age (make-config :type :range
     ;;                            :label "Person Age: ")
     ;;          :name (make-config :placeholder "(Name....)"))
     ;;    :form-class "container" )

     ;;   (create-form-from-object*
     ;;    body person (:age (:type :range :label "Person Age / v2 test: ")
     ;;                 :name (:placeholder "(Name HERE....)")
     ;;                 :phone (:validate-function
     ;;                         (lambda (val)
     ;;                           (every #'digit-char-p val)))))

     ;;   )
     ))
  (clog:open-browser)
)

