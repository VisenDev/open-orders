(open-orders.utils:defpackage* #:open-orders.class-ui
  (:use #:cl)
  (:import-from #:open-orders.utils
                #:fn)
  (:import-from #:introspect-environment
                #:typexpand)
  (:import-from #:defclass-std
                #:defclass/std
                #:class/std)
  (:local-nicknames (#:a #:alexandria)
                    (#:mop #:closer-mop)))
(in-package #:open-orders.class-ui)

(declaim (optimize (debug 3) (safety 3)))

(defclass/std config ()
  ((label)
   (value)))

(defclass/std config/toggle (config)
  ((style
    :std :checkbox
    :type (member :switch :checkbox))))

(defmethod slot-ui ((config config/toggle) container instance slot-name)
  (let* ((div (clog:create-div container))
         (label (clog:create-label div :content (label config)))
         (name (symbol-name (gensym "open-orders-toggle")))
         (input (clog:create-form-element
                 div :checkbox :role (if (eq (style config) :switch) "switch" "")
                 :name name
                 :label label
                 :value (if (value config) "on" "off"))))
    (clog:set-on-change
     input
     (fn (obj)
       (setf (slot-value instance slot-name)
             (clog:checkedp input))))))

(defclass/std config/filepicker (config) ())

(defmethod slot-ui ((config config/filepicker) container instance slot-name)
  (let* ((div (clog:create-div container))
         (label (clog:create-label div :content (label config)))
         (input (clog:create-form-element
                 div :file
                 :label label)))
    (clog:set-on-change
     input (fn (obj)
             (a:when-let (path (ignore-errors (pathname (clog:value input))))
               (setf (slot-value instance slot-name)
                     path))))))

(defclass/std config/text (config)
  ((placeholder :std "" :type string)))

(defmethod slot-ui ((config config/text) container instance slot-name)
  (let* ((div (clog:create-div container))
         (label (clog:create-label div :content (label config)))
         (input (clog:create-form-element
                 div :text
                 :label label
                 :value (if (value config) (value config) "")
                 :placeholder (placeholder config))))

    (clog:set-on-change
     input (fn (obj)
             (setf (slot-value instance slot-name)
                   (clog:value input))))))

(defclass config/integer (config)
  ((min :accessor min-value :initarg :min :initform nil)
   (max :accessor max-value :initarg :max :initform nil)))

(defmethod slot-ui ((config config/integer) container instance slot-name)
  (let* ((label (clog:create-label container :content (label config)))
         (args (list
                container :number
                :value (if (value config) (value config) 0)
                :label label)))
    (when (min-value config) (a:appendf args (list :min (min-value config))))
    (when (max-value config) (a:appendf args (list :max (max-value config))))
    (let ((input (apply #'clog:create-form-element args)))
      (clog:set-on-change
       input (fn (obj)
               (a:when-let (val (ignore-errors (parse-integer (clog:value input))))
                 (setf (slot-value instance slot-name) val)))))))

(defclass config/number (config)
  ((min :accessor min-value :initarg :min :initform nil)
   (max :accessor max-value :initarg :max :initform nil)))

(defmethod slot-ui ((config config/number) container instance slot-name)
  (let* ((label (clog:create-label container :content (label config)))
         (args (list
                container :number
                :value (if (value config) (value config) 0)
                :label label)))
    (when (min-value config) (a:appendf args (list :min (min-value config))))
    (when (max-value config) (a:appendf args (list :max (max-value config))))
    (let ((input (apply #'clog:create-form-element args)))
      (clog:set-on-change
       input (fn (obj)
               (a:when-let (val (ignore-errors
                                 (parse-float:parse-float (clog:value input) :type 'real)))
                 (setf (slot-value instance slot-name) val)))))))

(defclass config/slider (config)
  ((min :accessor min-value :initarg :min
        :initform (error "This slot is mandatory"))
   (max :accessor max-value :initarg :max
        :initform (error "This slot is mandatory"))))

(defmethod slot-ui ((config config/slider) container instance slot-name)
  (let* ((label (clog:create-label container :content (label config)))
         (input (clog:create-form-element container :range
                                          :label label
                                          :value (or (value config) 0)
                                          :min (min-value config)
                                          :max (max-value config))))
    
    (clog:set-on-change
     input (fn (obj)
             (a:when-let (val (ignore-errors
                               (parse-integer (clog:value input))))
               (setf (clog:inner-html label)
                     (format nil "~a: ~a" (label config) val))
               (setf (slot-value instance slot-name) val))))))

(defclass/std config/radio (config)
  ((options :type list)))

(defmethod slot-ui ((config config/radio) container instance slot-name)
  (let* ((div (clog:create-fieldset container))
         (legend (clog:create-legend div :content (label config)))
         (name (symbol-name (gensym "open-orders-radio")))
         (inputs
           (loop :for option :in (options config)
                 :for radio-label = (clog:create-label div :content option)
                 :collect
                 (clog:create-form-element
                  radio-label :radio :name name :auto-place :top))))
    (declare (ignore legend))
    (dolist (input inputs)
      (clog:set-on-change
       input (fn (obj)
               (setf (slot-value instance slot-name)
                     (clog:radio-value div name)))))))

(defclass/std config/color (config) ())

(defmethod slot-ui ((config config/color) container instance slot-name)
  (let* ((label (clog:create-label container :content (label config)))
         (input (clog:create-form-element container :color :label label)))

    (clog:set-on-change
     input (fn (obj)
             (setf (slot-value instance slot-name)
                   (clog:value input))))))

(defclass/std config/list (config)
  ((item-config :std (make-instance 'config/text :label ""))
   (adjustable :std t)
   (collapsible :std t)))
(class/std item-box value)

(defmethod slot-ui ((config config/list) container instance slot-name)
  (let* (;; (div (clog:create-div container))
         ;; (label (clog:create-label div :content (label config)))
         (details (clog:create-details container))
         (label (clog:create-summary details :content (label config))))
    (when (slot-boundp instance slot-name)
      ;; (unless (item-config config)
      ;;   (setf (item-config config) (make-instance 'config/text)))

      (dolist (item (slot-value instance slot-name))
        (let ((item-box (make-instance 'item-box :value item)))

          ;; TODO improve this so changes are actually forwarded to the things
          (slot-ui (item-config config) details item-box 'value))))
    )
  )


(defun diff (a b)
  "returns the difference between two numbers"
  (abs (- a b)))

(defgeneric finalize-config (config instance slotd))
(defmethod finalize-config ((config config) instance slotd)
  (let ((name (mop:slot-definition-name slotd))
        (type (typexpand (mop:slot-definition-type slotd))))
    (unless (label config)
      (setf (label config) (symbol-name name)))
    (unless (value config)
      (when (slot-boundp instance name)
        (setf (value config) (slot-value instance name))))
    
    (when (eq (class-of config) (find-class 'config))
      (cond
        ;; checkbox / toggle
        ((subtypep type 'boolean)
         (change-class config 'config/toggle))

        ;; radio
        ((and 
          (listp type)
          (eq 'member (first type)))
         (change-class config 'config/radio)
         (setf (options config) (rest type)))

        ;; file-author
        ((subtypep type 'pathname)
         (change-class config 'config/filepicker))

        ;; slider
        ((and (subtypep type 'integer)
              (listp type)
              (= 3 (length type))
              (numberp (second type))
              (numberp (third type))
              (< 1024 (diff (second type) (third type))))
         (change-class config 'config/slider :min (second type)
                                             :max (third type)))

        ;; integer
        ((subtypep type 'integer)
         (change-class config 'config/integer)
         (when (listp type)
           (when (and ( = (length type) 2)
                      (numberp (second type)))
             (setf (min-value config) (second type)))
           
           (when (and ( = (length type) 3)
                      (numberp (second type))
                      (numberp (third type)))
             (setf (min-value config) (second type))
             (setf (max-value config) (second type)))))

        ;; number
        ((subtypep type 'real)
         (change-class config 'config/number))

        ;; object
        ;; TODO

        ;; list
        ;; TODO

        ;; hash table
        ;; TODO

        (t
         (change-class config 'config/text))))))

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
  ((name hobbies siblings phone email address notes)
   (age :type integer)
   (in-prison :type boolean)
   (papers :type pathname)
   (favorite-color)))

(defvar *person* (make-instance 'person :name "John"))

(defun test ()

  (clog:initialize
   (lambda (body)

     ;; ;; Picocss
     (clog:load-css
      (clog:html-document body)
      "https://cdn.jsdelivr.net/npm/@picocss/pico@2.1.1/css/pico.min.css")


     ;; ;; Win 7
     ;; (clog:load-css (clog:html-document body)
     ;;                "https://unpkg.com/7.css")

     ;; ;; Apple Classic
     ;; (clog:load-css (clog:html-document body)
     ;;                "https://unpkg.com/@sakun/system.css")

     ;; NES
     ;; (clog:load-css (clog:html-document body)
     ;;                "https://unpkg.com/nes.css@latest/css/nes.min.css")

     ;; XP
     ;; (clog:load-css (clog:html-document body)
     ;;                "https://unpkg.com/xp.css")

     ;; 98
     ;; (clog:load-css (clog:html-document body)
     ;;                "https://unpkg.com/xp.css@0.2.3/dist/98.css")

     ;; PS1
     ; (clog:l
     ;; (clog:
     ;; (clog:load-css (clog:html-document body)
     ;;                "https://cdn.jsdelivr.net/gh/98mprice/PSone.css@master/PSone.min.css")

     (class-ui (list :email (make-instance 'config/text :placeholder "foo@foo.com")
                     :age (make-instance 'config/slider :min 0 :max 100)
                     :siblings (make-instance 'config/radio
                                              :options '(one two three four five+))
                     :favorite-color (make-instance 'config/color)
                     :hobbies (make-instance 'config/list))
               *person* body)
       

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

