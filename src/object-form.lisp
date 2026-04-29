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

(deftype element-type () '(member :slider :text :number))

(defstruct config
  (type "")
  (min 0 :type integer)
  (max 100 :type integer)
  (placeholder "" :type string)
  (class "" :type string)
  label)

(defun create-form-from-object (clog-obj instance slot-config-plist &key (form-class ""))
  (loop
    :with form = (clog:create-form clog-obj :class form-class)
    :for slot :in (mop:class-slots (class-of instance))
    :for name = (mop:slot-definition-name slot)
    :for config = (or (getf slot-config-plist name)
                      (getf slot-config-plist (a:make-keyword name))
                      (make-config))
    :for _ = (unless (config-label config) (setf (config-label config) name))
    :for div = (clog:create-div form)
    :for label = (clog:create-label div :content (config-label config))
    :for form-element = (clog:create-form-element
                         div (config-type config)
                         :placeholder (config-placeholder config))
    :do (clog:label-for label form-element)
        (a:when-let (val (and (slot-boundp instance name)
                              (slot-value instance name)))
          (setf (clog:value form-element) (format nil "~a" val)))
        
        (let ((form-element form-element)
              (instance instance))
          (clog:set-on-change
           form-element
           (lambda (obj)
             (declare (ignore obj))
             (setf (slot-value instance name)
                   (clog:value form-element)))))
    :collect form-element))

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
     (let ((person (make-instance 'person :name "Bobby")))
       (create-form-from-object
        body person
        (list :age (make-config :type :range :label "Person Age: ")
              :name (make-config :placeholder "(Name....)")))

       (create-form-from-object*
        body person (:age (:type :range :label "Person Age / v2 test: ")
                     :name (:placeholder "(Name HERE....)")))

       )))
  (clog:open-browser)
)

