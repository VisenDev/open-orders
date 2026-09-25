(defpackage #:open-orders.derive-page
  (:use #:cl
        #:open-orders.html-generator
        #:open-orders.database
        #:open-orders.templates
        #:open-orders.auth
        #:open-orders.fn
        )
  (:import-from #:url-rewrite
                #:url-encode)
  (:export
   #:geta
   #:derive-list-page-from-table
   #:table-url
   #:derive-new-page-from-table
   #:derive-save-page-from-table
   #:define-edit-page-for-table
   #:derive-edit-page-from-table))
(in-package #:open-orders.derive-page)

(defparameter *max-columns-on-mobile* 2)

(fn (geta t) (item (alist list) &key (test #'equal))
  "Alist equivalent to getf"
  (cdr (assoc item alist :test test)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (fn (table-url string) ((def table) (page (or string symbol)))
    "Get the url for a specific table page, and format GET parameters"
    (string-downcase (format nil "/~a/~a" (table-namestring def) page)))

  (fn (generate-table-url t)
      ((def table) (page (or string symbol)) &rest parameter-plist)
    (if parameter-plist
        `(format nil ,(apply #'concatenate 'string (table-url def page)
                             (when parameter-plist "?")
                             (loop :for (key value) :on parameter-plist :by #'cddr
                                   :appending (list (string-downcase
                                                     (format nil "~a=~~a" key))
                                                    "&")
                                     :into forms
                                   :finally (return (butlast forms))))
                 ,@(loop :for (key value) :on parameter-plist :by #'cddr
                         :collect (if (stringp value)
                                      (url-encode value)
                                      `(url-encode
                                        (format nil "~a" ,value)))))
        (table-url def page))))

(defmacro derive-list-page-from-table (table-name &key (create-toplevel-link t))
  (let* ((def (find-table table-name))
         (listed-fields (remove-if-not
                         (lambda (field)
                           (getf (field-metadata field) :show-in-list-view-p))
                         (table-fields def))))
    `(progn

       ;; Add Toplevel Url
       ,(when create-toplevel-link
          `(pushnew (make-tab :name ,(format nil " [~a] " (table-namestring def))
                              :url ,(generate-table-url def "list"))
                    *toplevel-tabs* :test #'equalp))

       (hunchentoot:define-easy-handler
           (,(open-orders.fn:symbolicate table-name '-list)
            :uri ,(generate-table-url def "list"))
           (sort-by reverse search clear)
         (when clear (setf search nil))
         (let ((mobilep (mobile-browser-p))
               (new-form
                 (td ()
                   (form (:action ,(format nil "/~a/new" (table-namestring def)))
                     (input
                      (:type "submit"
                       :value ,(format nil "new ~a" (table-namestring def)))))))
               (list-form (td ()
                            (form (:action ,(table-url def "list"))
                              (input (:type "text"
                                      :name "search"
                                      :value (if search search "")))
                              (input (:type "submit"
                                      :value "Search"))
                              (when search
                                (input (:type "submit"
                                        :name "clear"
                                        :value "Clear")))))))
           (declare (ignorable mobilep))
           (with-internal-page
             (hr ())
             (html-table ()
               (if mobilep
                   (list (tr () new-form)
                         (tr () list-form))
                   (tr ()
                     new-form list-form)))
             (hr ())
             (html-table (:class "border")

               ;; table header
               (tr ()
                 ,@(loop :for field :in listed-fields
                         :for i :from 0
                         :collect
                         `(unless ,(if (< i *max-columns-on-mobile*)
                                       nil
                                       'mobilep)
                            (th ()
                              (a (:href
                                  (if
                                   search
                                   ,(generate-table-url
                                     def "list"
                                     :sort-by (field-namestring field)
                                     :reverse '(if (string= reverse "true")
                                                "false" "true")
                                     :search 'search)
                                   ,(generate-table-url
                                     def "list"
                                     :sort-by (field-namestring field)
                                     :reverse '(if (string= reverse "true")
                                                "false" "true"))))
                                ,(format nil "[~a]" (field-namestring field)))))))

               ;; table body
               (loop
                 :for val
                   :across
                   (let* ((sorted (sort (if search
                                            (remove-if-not
                                             (lambda (value)
                                               (search
                                                search
                                                (format nil "~a" value)))
                                             (,(table-get-every-function def)))
                                            (,(table-get-every-function def)))
                                        (lambda (a b)
                                          ;; todo, use the fields handler
                                          (string< (format nil "~a" a)
                                                   (format nil "~a" b)))
                                        :key
                                        (cond 
                                          ,@(mapcar
                                             (lambda (f)
                                               `((string= sort-by
                                                          ,(field-namestring f))
                                                 (function ,(field-accessor f)))
                                               )
                                             (table-fields def)))))
                          (reversed (if (string= reverse "true")
                                        (nreverse sorted)
                                        sorted)))
                     reversed)
                 :collect
                 (tr ()
                   ,@(loop
                       :for field :in listed-fields
                       :for i :from 0
                       :collect
                       `(unless ,(if (< i *max-columns-on-mobile*)
                                     nil
                                     'mobilep)
                          (td ()
                            (a (:href ,(generate-table-url
                                        def
                                        "edit"
                                        :id `(,(table-id-accessor def) val)))
                              ,(let* ((display-as (getf (field-metadata field)
                                                        :display-as))
                                      (reference-def
                                        (find-table
                                         (field-references field))))

                                 ;; DISPLAY AS AND REFERENCES
                                 (cond
                                   
                                   ((and reference-def display-as)
                                    `(ignore-errors
                                      (,display-as
                                       (,(table-get-function reference-def)
                                        (,(field-accessor field) val)))))
                                   
                                   (reference-def
                                    `(,(table-get-function reference-def)
                                      (,(field-accessor field) val)))

                                   (display-as
                                    `(ignore-errors
                                      (,display-as
                                       (,(field-accessor field) val))))

                                   (t
                                    `(,(field-accessor field) val)))))))))))))))))

(defmacro derive-new-page-from-table (table-name)
  (let ((def (find-table table-name)))
    `(hunchentoot:define-easy-handler
         (,(open-orders.fn:symbolicate table-name '-new)
          :uri ,(table-url def "new"))
         ()
       (perform-auth-check)
       (let* ((new (,(table-constructor def)))
              (id (,(table-set-function def) new)))
         (hunchentoot:redirect ,(generate-table-url def "edit" :id 'id)
                               :code 303)))))

(defun coerce-form-data-to-type (form-data-string type)
  (cond
    ((eq type 'date)
     (or (ignore-errors
          (let ((year (subseq form-data-string 0 4))
                (month (subseq form-data-string 5 7))
                (day (subseq form-data-string 8 10)))
            (encode-universal-time
             0 0 0
             (parse-integer day)
             (parse-integer month)
             (parse-integer year))
            (get-universal-time)))
         (random (get-universal-time))))
    ((subtypep type 'integer)
     (parse-integer form-data-string :junk-allowed t))
    ((subtypep type 'boolean)
     (string= "on" form-data-string))
    ((or (subtypep type 'string)
         (eq type t))
     form-data-string)
    (t
     (error "Don't know how to convert the type '~a' from a string" type))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun generate-form-deserializer-for-field (post-parameters-varname
                                               table-value-varname field)
    `(let ((field-form-value (geta ,(field-namestring field)
                                   ,post-parameters-varname)))
       (when field-form-value
         (setf (,(field-accessor field) ,table-value-varname)
               (coerce-form-data-to-type field-form-value
                                         ',(if (field-references field)
                                               'integer
                                               (field-type field))))))))

(defmacro derive-save-page-from-table (table-name)
  (let ((def (find-table table-name)))
    `(hunchentoot:define-easy-handler
         (,(open-orders.fn:symbolicate table-name '-save)
          :uri ,(table-url def "save"))
         (id)
       
       (let* ((params (hunchentoot:post-parameters*))
              (id (parse-integer id))
              (redirect-url (geta "redirect-url" params))
              (val (,(table-get-function def) id)))

         ;; iterate over all fields, getting their values from
         ;; parameters and setting them when non-null
         ,@(mapcar (lambda (field) (generate-form-deserializer-for-field
                                    'params 'val field))
                   
                   (remove "id" (table-fields def) :key #'field-namestring
                                                   :test #'string=))

         ;; Save Value
         (,(table-set-function def) val)

         ;; Redirect (get, post, redirect pattern)
         (hunchentoot:redirect redirect-url :code 303)))))

(defmacro define-edit-page-for-table (table-name get-parameters
                                      (save-endpoint-variable edit-value-variable)
                                      &body body)
  (let ((def (find-table table-name)))
    `(hunchentoot:define-easy-handler
         (,(open-orders.fn:symbolicate table-name '-edit)
          :uri ,(table-url def "edit"))
         (id ,@get-parameters)
       (let ((,save-endpoint-variable ,(generate-table-url def "save" :id 'id))
             (,edit-value-variable (,(table-get-function def) (parse-integer id))))
         (declare (ignorable ,save-endpoint-variable ,edit-value-variable))
         ,@body))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun generate-form-input-from-field (&key namestring references
                                           metadata type value)
    (tr ()
       (td () namestring)
       (td ()
         (if references

             ;; Dropdown for foreign tables
             (select (:name namestring)
               ;; Foreign table definition lookup
               (loop
                 :with foreign-def = (find-table references)
                 :with get-every = (table-get-every-function foreign-def)
                 :for foreign-table-value :across (funcall get-every)
                 :for foreign-id
                   = (funcall (table-id-accessor foreign-def)
                              foreign-table-value)

                 :for option-body =
                                  (if (getf metadata :display-as)
                                      (ignore-errors
                                       (funcall (getf metadata :display-as)
                                                foreign-table-value))
                                      foreign-table-value)
                                  
                                  ;; collect html options for each foreign value
                 :collect

                 ;; eql not '=', since foreign id may
                 ;; be nil
                 (if (eql foreign-id value
                          ;; (,(field-accessor field)
                          ;;  table-value)
                          )
                     (option (:selected "selected"
                              :value foreign-id)
                       option-body)

                     ;; else the id is not the currently
                     ;; chosen id
                     (option (:value foreign-id)
                       option-body))))
             
             ;; else if the field doesn't reference any table
             ;; Just make it an input not a dropdown
             (input (:name namestring
                      :value value
                      :type (cond
                              ((eq type 'date) "date")
                              ((subtypep type 'number) "number")
                              ((subtypep type 'boolean) "checkbox")
                              (t "text")))))))))

(defmacro derive-edit-page-from-table (table-name)
  (let ((def (find-table table-name))) 
    `(define-edit-page-for-table ,table-name () (save-url table-value)
       (with-internal-page
         (hr ())
         (form (:method "post" :action save-url)
           (html-table ()
             (tr ()
               (td ()
                 (Button (:type "submit" :name "redirect-url"
                          :value ,(table-url def "list"))
                   "back"))
               (td ()
                 (button (:type "submit" :name "redirect-url"
                          :value (hunchentoot:request-uri*))
                   "save"))
               (td ()
                 (button (:command "show-modal"
                          :commandfor "confirm-delete"
                          :type "button")
                   "delete"))))
           (hr ())
           (html-table ()

             ;; Create a edit row for table field
             ,@(loop :for field :in (remove "id" (table-fields def)
                                            :key #'field-namestring
                                            :test #'string=)
                     :collect
                     `(generate-form-input-from-field
                       :namestring ,(field-namestring field)
                       :metadata ',(field-metadata field)
                       :references ',(field-references field)
                       :type ',(field-type field)
                       :value (,(field-accessor field) table-value))))

           ;; delete modal for deleting a record
           (dialog (:id "confirm-delete")
             (button ()
               "Yes, I want to delete this")
             (button (:command "close"
                      :commandfor "confirm-delete"
                      :type "button")
               "Cancel")))))))

