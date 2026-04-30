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

(deftype element-type ()
  `(member :button
           :checkbox
           :color
           :date
           :datetime-local
           :email
           :file
           :hidden
           :image
           :month
           :number
           :password
           :radio
           :range
           :reset
           :search
           :submit
           :tel
           :text
           :time
           :url
           :week))

(declaim (ftype (function (t) string) to-string))
(defun to-string (object)
  (format nil "~a" object))

(defstruct (config (:conc-name config.))
  (type :text :type element-type)
  (min nil :type (or integer null))
  (max nil :type (or integer null))
  (placeholder "" :type string)
  (class "" :type string)
  (label nil :type (or string null))
  (validate-function (constantly t))
  (lisp-to-element #'to-string)
  (element-to-lisp #'identity))

(defun create-form-from-object (clog-obj instance slot-config-plist &key (form-class ""))
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
                          (setf (clog:color form-element) :red))   ;;; TODO, find a better way to show validity
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

     
     (let ((person (make-instance 'person :name "Bobby")))
       (create-form-from-object
        body person
        (list :age (make-config :type :range
                                :label "Person Age: ")
              :name (make-config :placeholder "(Name....)"))
        :form-class "container" )

       (create-form-from-object*
        body person (:age (:type :range :label "Person Age / v2 test: ")
                     :name (:placeholder "(Name HERE....)")
                     :phone (:validate-function
                             (lambda (val)
                               (every #'digit-char-p val)))))

       )))
  (clog:open-browser)
)

