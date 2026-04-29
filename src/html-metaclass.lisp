(defpackage #:open-orders.html-form
    (:use #:cl)
  (:import-from #:defclass-std
                #:defclass/std
                #:class/std)
    (:import-from #:open-orders.utils
                #:fn
                #:*let)
  (:local-nicknames (#:a #:alexandria)
                    (#:mop #:closer-mop)
                    ;; (#:db #:open-orders.db)
                    )
  (:shadow #:type #:min #:max))
(in-package #:open-orders.html-form)

(deftype input-type ()
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
           :week
           :foreign-key-edit))

(defclass html-form (standard-class) ())

(defclass/std html-form-effective-slot-definition (mop:standard-effective-slot-definition)
  ((input-type :std :text)
   (name label min max pattern placeholder)))

(defclass/std html-form-direct-slot-definition (mop:standard-direct-slot-definition)
  ((input-type name label min max pattern placeholder)))

(defmethod mop:validate-superclass ((class html-form) (superclass standard-class))
  t)

(defmethod mop:effective-slot-definition-class ((class html-form) &rest initargs)
  (declare (ignore initargs))
  (find-class 'html-form-effective-slot-definition))

(defmethod mop:direct-slot-definition-class ((class html-form) &rest initargs)
  (declare (ignore initargs))
  (find-class 'html-form-direct-slot-definition))

(defmethod mop:compute-effective-slot-definition ((class html-form) name direct-slots)
  (let ((eslot (call-next-method)))
    (setf (input-type eslot)
          (some #'input-type direct-slots))
    (setf (name eslot)
          (some #'name direct-slots))
    (setf (label eslot)
          (some #'label direct-slots))
    (setf (min eslot)
          (some #'min direct-slots))
    (setf (max eslot)
          (some #'max direct-slots))
    (setf (pattern eslot)
          (some #'pattern direct-slots))
    (setf (placeholder eslot)
          (some #'placeholder direct-slots))

    eslot))

(defun create-form-from-object (body instance)
  (let ((form (clog:create-form body))
        (class (class-of instance)))
    (loop :for slot :in (mop:class-slots class)
          :for input-type = (or (ignore-errors (input-type slot)) :text)
          :for label = (or (ignore-errors (label slot))
                          (mop:slot-definition-name slot))
          :for label-elem = (clog:create-label form :content label)
          :for elem = (clog:create-form-element
                       form input-type)
          ;; :for value = nil
          ;; :when (eq input-type :range)
          ;;   :do (setf value (clog:create-p form :content "50"))
          ;; :end
          :do (clog:create-br form)
             (clog:label-for label-elem elem)
              (let ((slot slot)
                    (elem elem)
                    (input-type input-type)
                    (instance instance)
                    (label-elem label-elem))
                (clog:set-on-change
                 elem
                 (fn (obj)
                   (setf (slot-value instance
                                     (mop:slot-definition-name slot))
                         (clog:value elem))
                   (when (eq input-type :range)
                     (setf (clog:inner-html label-elem)
                           (format nil "~a:  ~a"
                                   (mop:slot-definition-name slot)
                           (slot-value instance
                                     (mop:slot-definition-name slot)))))))))
    ;; (clog:set-on-mouse-click form (fn (obj event) (format t "~a"
    ;;                                                 (age instance)
    ;;                                                 )))
    ))


(defclass test-person ()
  ((age :accessor age :initarg :age :input-type :range :min 1 :max 100)
   (name)
   (weight)
   (title)
   (friends))
  (:metaclass html-form))

(defun test ()
  (clog:initialize
   (lambda (body)
     (clog:load-css (clog:html-document body)
                    "https://cdn.jsdelivr.net/npm/@picocss/pico@2.1.1/css/pico.min.css")
     (let ((p (make-instance 'test-person :age 20)))
       (create-form-from-object (clog:create-div body :class "container") p))))
  (clog:open-browser))


;; ;; maybe do this as a function not a metaclass


;; (defclass/std input ()
;;   ((input-type name label min-value max-value default-value regex-pattern placeholder required)))

;; xo(defun create-form-from-object (body instance settings)
;;   )

;; (create-form-from-object body (make-instance 'person)
;;                          '(:email (:type "email")
;;                            :phone (:type "tel")
;;                            :notes (:type "textarea")))
