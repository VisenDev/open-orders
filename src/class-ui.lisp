(open-orders.utils:defpackage* #:open-orders.class-ui
  (:use #:cl)
  (:import-from #:open-orders.utils
                #:fn)
  (:import-from #:defclass-std
                #:defclass/std
                #:class/std)
  (:local-nicknames (#:a #:alexandria)
                    (#:mop #:closer-mop)))
(in-package #:open-orders.class-ui)

(declaim (optimize (debug 3) (safety 3)))


;; input types
;;    toggle
;;    checkbox
;;    filepicker
;;    text-input
;;    integer-input
;;    decimal-input
;;    slider
;;    color-picker
;;    date-picker

(defclass/std config ()
  ((label)
   (value)))

(defclass/std config/toggle (config)
  ((type :std :switch :type (member :switch :checkbox))))

(defmethod slot-ui ((config config/toggle) container instance slot-name)
  (let* ((label (clog:create-label container :content (label config)))
         (name (symbol-name (gensym "open-orders-toggle")))
         (input (clog:create-form-element
                 container :checkbox :role (if (eq type :switch) "switch" "")
                 :name name
                 :value (if (value config) "on" "off"))))

    (clog:label-for label input)
    (clog:set-on-input
     input (fn (obj)
             (setf (slot-value instance slot-name)
                   (clog:checkbox-value input name))))))

(defclass/std config/filepicker (config) ())

(defclass/std config/text (config)
  ((placeholder :std "" :type string)))

(defmethod slot-ui ((config config/text) container instance slot-name)
  (let* ((label (clog:create-label container :content (label config)))
         (input (clog:create-form-element
                 container :text
                 :value (if (value config) (value config) "")
                 :placeholder (placeholder config))))

    (clog:label-for label input)
    (clog:set-on-input
     input (fn (obj)
             (setf (slot-value instance slot-name)
                   (clog:value input))))))

(defclass config/integer (config)
  ((min :accessor min-value :initarg :min :initform nil)
   (max :accessor max-value :initarg :max :initform nil)))

(defclass config/number (config)
  ((min :accessor min-value :initarg :min :initform nil)
   (max :accessor max-value :initarg :max :initform nil)))

(defclass config/slider (config)
  ((min :accessor min-value :initarg :min :initform nil)
   (max :accessor max-value :initarg :max :initform nil)))

(defclass/std config/radio (config)
  ((options :type list)))

;; TODO create config/radio slot-ui method


(defparameter *boolean-default-config* 'config/checkbox)

(defgeneric finalize-config (config instance slotd))
(defmethod finalize-config ((config config) instance slotd)
  (let ((name (mop:slot-definition-name slotd))
        (type (mop:slot-definition-type slotd)))
    (unless (label config)
      (setf (label config) (symbol-name name)))
    (unless (value config)
      (when (slot-boundp instance name ))
      (setf (value config) (slot-value instance name)))
    
    (when (eq (class-of config) (find-class 'config))
      (cond
        ;; checkbox / toggle
        ((subtypep type 'boolean)
         (setf config
               (change-class config *boolean-default-config*)))

        ;; radio
        ((and 
          (listp type)
          (eq 'member (first type)))
         (setf config
               (change-class config 'config/radio))
         (setf (options config) (rest type)))

        ;; file-author
        ((subtypep type 'pathname)
         (setf config
               (change-class config 'config/filepicker)))

        ;; integer
        ((subtypep type 'integer)
         (error "TODO")
         ;; (if (and (config.min config)
         ;;          (config.max config))
         ;;     (setf (config.type config) :range)
         ;;     ;;else
         ;;     (progn
         ;;       (setf (config.type config) :text)
         ;;       (unless (config.pattern config)
         ;;         (setf (config.pattern config) "/[0-9]*/"))))
         )

        ;; number
        ((subtypep type 'real)
         (error "TODO"))

        (t
         (setf config
               (change-class config 'config/text)))))))

;; (defmethod initialize-config ((config config) instance slotd)
;;   (unless (label config)
;;     (setf (label config) (symbol-name (mop:slot-definition-name slot-name))))

;;   ;; handle element type
 
;;   )


(defun getf* (plist symbol &optional default)
  "getf but it also checks using the keyword version of <symbol>"
  (getf plist symbol
        (getf plist (a:make-keyword symbol)
              default)))

(defparameter *form-class* "container")

(defmethod class-ui (slot-config-plist
                      (instance standard-object) (container clog:clog-obj))
  (let ((form (clog:create-form container :class *form-class*)))
    (dolist (slotd (mop:class-slots (class-of instance)))
      (let* ((name (mop:slot-definition-name slotd))
             (config (getf* slot-config-plist name (make-instance 'config))))
        (finalize-config config instance slotd)
        (slot-ui config form instance name)))))



(defclass/std person ()
  ((age name hobbies siblings phone email address notes)
   (in-prison :type boolean)))

(defun test ()

  (clog:initialize
   (lambda (body)

     (clog:load-css
      (clog:html-document body)
      "https://cdn.jsdelivr.net/npm/@picocss/pico@2.1.1/css/pico.min.css")

       (let ((p (make-instance 'person :name "John")))
         (class-ui (list :email (make-instance 'config/text :placeholder "foo@foo.com"))
                   p body)
    )

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

