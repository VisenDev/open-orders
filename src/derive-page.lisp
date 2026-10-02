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
   #:make-derive-page-config
   #:derive-page-config
   #:page-config
   #:geta
   #:derive-list-page-from-table
   #:table-url
   #:derive-new-page-from-table
   #:derive-save-page-from-table
   #:define-edit-page-for-table
   #:derive-edit-page-from-table
   #:derive-delete-page-from-table
   #:derive-view-reference-page-from-table
   #:derive-set-field-page-from-table))
(in-package #:open-orders.derive-page)

(defparameter *max-columns-on-mobile* 2)

(defvar *hunchentoot-dispatchers* (make-hash-table :test 'equal))
(fn (register-page t) ((url string) (callback (function () t)))
  "Registers a hunchentoot dispatcher match url"
  (let ((existing-dispatcher (gethash url *hunchentoot-dispatchers*)))
    (when (not (null existing-dispatcher))
      (setf hunchentoot:*dispatch-table*
            (delete existing-dispatcher hunchentoot:*dispatch-table*))))

  (let ((dispatcher (hunchentoot:create-prefix-dispatcher url callback)))
    (setf (gethash url *hunchentoot-dispatchers*) dispatcher)
    (push dispatcher hunchentoot:*dispatch-table*)))

(defstruct (page-config (:conc-name config-)
                        (:constructor page-config))
  (show-in-list-view-p nil :type boolean)
  (display-as nil #|:type (function (t) string)|#)
  (display-name nil :type (or string null))
  (compare-function nil #|:type (function (t t) boolean)|#)
  (suggested-values nil :type list)
  (edit-ui-generator nil #|:type (function (field-value) t)|#))

(fn (geta t) (item (alist list) &key (test #'equal))
  "Alist equivalent to getf"
  (cdr (assoc item alist :test test)))

(fn (default-compare-function boolean) ((a t) (b t))
  (not (not (string< (format nil "~a" a)
                     (format nil "~a" b)))))

(fn (table-url string) ((def table) (page (or string symbol)) &rest params-alist)
  "Get the url for a specific table page, and format GET parameters"
  (apply #'concatenate 'string (string-downcase
                                (format nil "/~a/~a"
                                        (table-namestring def) page))
         (when params-alist "?")
         (butlast (loop :for (key . value) :in (remove nil params-alist)
                        :collect (format nil "~a=~a"
                                         (string-downcase
                                          (url-encode (format nil "~a" key)))
                                         (url-encode (format nil "~a" value)))
                        :collect "&"))))

(fn (get-page-config (or page-config null)) ((field field))
  (let ((config (getf (field-metadata field) :page-config)))

    ;; Construct config declaratively 
    (cond (config
           (assert (eq (first config) 'page-config))
           (apply #'page-config (rest config)))
          (t
           (page-config)))))

(fn (get-every-table-value-filtered (or vector null))
      ((table-name symbol)
       (sort-by (or string null))
       (search (or string null))
       (reverse (or string null)))
    (let* ((def (find-table table-name))
           (raw (funcall (table-get-every-function def)))
           (filtered
             (if search
                 (remove-if-not
                  (lambda (value)
                    (search search (format nil "~a" value)))
                  raw)
                 raw))
           (field
             (and sort-by
                  (find sort-by
                        (table-fields def)
                        :key #'field-namestring
                        :test #'string=)))
           (sorted
             (if field
                 (sort filtered
                       (or (config-compare-function
                            (get-page-config field))
                           #'default-compare-function)
                       :key (field-accessor field))
                 filtered)))
      (if (string= reverse "true")
          (nreverse sorted)
          sorted)))

(defmacro lambda-with-parameters (parameters &body body)
  "Creates a lambda that binds the variable list parameters to http 
   parameters in its body"
  `(lambda ()
     (let ,(mapcar (lambda (param)
                     `(,param (hunchentoot:parameter ,(string-downcase
                                                       (symbol-name param)))))
            parameters)
       ,@body)))

(fn (get-field-display-value t) ((field field) (table-value t))
  "Convert a table field into what it should be displayed as in html"
  (let* ((display-as (config-display-as
                      (get-page-config field)))
         (reference-def
           (find-table
            (field-references field))))

    (cond
      
      ((and reference-def display-as)
       (ignore-errors
        (funcall
         display-as
         (funcall (table-get-function reference-def)
                  (funcall (field-accessor field) table-value)))))
      
      (reference-def
       (funcall (table-get-function reference-def)
                (funcall (field-accessor field) table-value)))

      (display-as
       (ignore-errors
        (funcall display-as
                 (funcall (field-accessor field) table-value))))

      (t
       (funcall (field-accessor field) table-value)))))

(defun derive-list-page-from-table (table-name &key (create-toplevel-link t))
  (let* ((def (find-table table-name))
         (listed-fields (remove-if-not
                         (lambda (field)
                           (config-show-in-list-view-p (get-page-config field)))
                         (table-fields def))))

    ;; Add Toplevel Url
    (when create-toplevel-link
      (pushnew (make-tab :name (format nil " [~a] " (table-namestring def))
                         :url (table-url def "list"))
               *toplevel-tabs* :test #'equalp))

    ;; Register Page Handler
    (register-page
     (table-url def "list")
     (lambda-with-parameters (sort-by reverse search clear)
       (when clear (setf search nil))
       (let ((new-form
               (td ()
                 (form (:action (table-url def "new"))
                   (input
                    (:type "submit"
                     :value (format nil "new ~a" (table-namestring def)))))))
             (list-form (td ()
                          (form (:action (table-url def "list"))
                            (input (:type "text"
                                    :name "search"
                                    :value (if search search "")))
                            (input (:type "submit"
                                    :value "Search"))
                            (when search
                              (input (:type "submit"
                                      :name "clear"
                                      :value "Clear")))))))
         (with-internal-page
           (hr ())
           (html-table ()
             (if (mobile-browser-p)
                 (list (tr () new-form)
                       (tr () list-form))
                 (tr ()
                   new-form list-form)))
           (hr ())
           (html-table (:class "border")

             ;; table header
             (tr ()
               (remove
                nil
                (loop
                  :for field :in listed-fields
                  :for i :from 0 :below (if (mobile-browser-p)
                                            *max-columns-on-mobile*
                                            (length listed-fields))
                  :collect
                  (th ()
                    (a (:href
                        (table-url
                         def "list"
                         (cons :sort-by (field-namestring field))
                         (cons :reverse (if (string= reverse "true")
                                            "false" "true"))
                         (when search (cons :search search))))
                      
                      (format nil "[~a]"
                              (or (config-display-name (get-page-config field))
                                  (field-namestring field))))))))

             ;; table body
             (loop
               :for val :across (get-every-table-value-filtered
                                 table-name sort-by search reverse)
               :collect
               (tr ()
                 (loop
                   :for field :in listed-fields
                   :for i :from 0 :below (if (mobile-browser-p)
                                             *max-columns-on-mobile*
                                             (length listed-fields))
                   :collect
                   (td ()
                     (a (:href (table-url
                                def "edit"
                                (cons :id (funcall
                                           (table-id-accessor def) val))))
                       (let ((display-value (get-field-display-value field val)))
                         (if (or (null display-value)
                                 (and (stringp display-value)
                                      (string-equal display-value "")))
                             ""
                             ;; "<i>&ltempty&gt</i>"
                             display-value)
                         )))))))))))))

(defun derive-new-page-from-table (table-name)
  (let ((def (find-table table-name)))
    (register-page
     (table-url def "new")
     (lambda-with-parameters (back-url)
       (perform-auth-check)
       (let* ((new (funcall (table-constructor def)))
              (id (funcall (table-set-function def) new)))
         (hunchentoot:redirect (table-url def "edit" (cons :id id)
                                          (cons :back-url back-url))
                               :code 303))))))

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
             (parse-integer year))))
         (random (get-universal-time))))
    ((eq type 'list)
     (let ((cl:*read-eval* nil))
       (read-from-string form-data-string)))
    ((subtypep type 'integer)
     (parse-integer form-data-string :junk-allowed t))
    ((subtypep type 'boolean)
     (string= "on" form-data-string))
    ((or (subtypep type 'string)
         (eq type t))
     form-data-string)
    (t
     (error "Don't know how to convert the type '~a' from a string" type))))

(fn (deserialize-field-from-http-parameter t) ((field-form-value (or null string))
                                                (table-value t)
                                                (field field))
  (when field-form-value
    (funcall
     (fdefinition `(setf ,(field-accessor field)))
     (coerce-form-data-to-type field-form-value
                               (if (field-references field)
                                   'integer
                                   (field-type field)))
     table-value)))

(defun derive-set-field-page-from-table (table-name)
  (let* ((def (find-table table-name)))
    (register-page
     (table-url def "set-field")
     (lambda-with-parameters (id field-namestring value redirect-url)
       (let* ((id (parse-integer id :junk-allowed t))
              (table-val (funcall (table-get-function def) id))
              (field (find field-namestring (table-fields def)
                           :test #'string=
                           :key #'field-namestring)))

         (assert field)

         (deserialize-field-from-http-parameter value table-val field)
         
         ;; Save Value
         (funcall (table-set-function def) table-val)

         ;; Redirect (get, post, redirect pattern)
         (hunchentoot:redirect redirect-url :code 303))))))

(defun derive-save-page-from-table (table-name)
  (let* ((def (find-table table-name)))
    (register-page
     (table-url def "save")
     (lambda-with-parameters (id)
       (let* ((params (hunchentoot:post-parameters*))
              (id (parse-integer id :junk-allowed t))
              (redirect-url (geta "redirect-url" params))
              (val (funcall (table-get-function def) id)))

         ;; iterate over all fields, getting their values from
         ;; parameters and setting them when non-null
         (mapcar (lambda (field) (deserialize-field-from-http-parameter
                                  (geta (field-namestring field) params)
                                  val field))
                 
                 (remove "id" (table-fields def) :key #'field-namestring
                                                 :test #'string=))

         ;; Save Value
         (funcall (table-set-function def) val)

         ;; Redirect (get, post, redirect pattern)
         (hunchentoot:redirect redirect-url :code 303))))))

(fn (generate-form-input-from-field t) (&key ((id integer))
                                             ((def table))
                                             ((field field))
                                             value)
  (let* ((references (field-references field))
         (foreign-def  (find-table references))
         (namestring (field-namestring field))
         (page-config (get-page-config field))
         (type (field-type field))
         (input-form 
           (cond

             ((config-edit-ui-generator page-config)
              (funcall (config-edit-ui-generator page-config) id def field value))

             ((not (null references))
              ;; Dropdown for foreign tables
              (list
               
               (select (:name namestring)
                 ;; Foreign table definition lookup
                 (loop
                   :with get-every = (table-get-every-function foreign-def)
                   :with all-foreign-values = (funcall get-every)
                   :with sorted-values = (sort all-foreign-values #'string<
                                               :key (if (config-display-as page-config)
                                                        (config-display-as page-config)
                                                        (lambda (thing)
                                                          (format nil "~a" thing))))
                   :for foreign-table-value :across sorted-values
                   :for foreign-id
                     = (funcall (table-id-accessor foreign-def)
                                foreign-table-value)

                   :for option-body =
                                    (if (config-display-as page-config)
                                        (ignore-errors
                                         (funcall (config-display-as page-config)
                                                  foreign-table-value))
                                        foreign-table-value)
                                    
                                    ;; collect html options for each foreign value
                   :collect

                   ;; eql not '=', since foreign id may
                   ;; be nil
                   (if (eql foreign-id value)
                       (option (:selected "selected"
                                :value foreign-id)
                         option-body)

                       ;; else the id is not the currently
                       ;; chosen id
                       (option (:value foreign-id)
                         option-body))))

               ;; View Foreign Table Value Button
               (button (:type "submit"
                        :name "redirect-url"
                        :value (table-url def "view-reference"
                                          (cons :id id)
                                          (cons :field-name namestring)
                                          (cons :back-url (hunchentoot:request-uri*))))
                 "View")

               ;; New reference button
               (button (:type "submit"
                        :name "redirect-url"
                        :value (table-url foreign-def "new"
                                          (cons :back-url (hunchentoot:request-uri*))))
                 "New")))

             (t
              
              ;; else if the field doesn't reference any table
              ;; Just make it an input not a dropdown
              (remove
               nil
               (list
                (input (:name namestring
                        :value (cond
                                 ((eq type 'date)
                                  (multiple-value-bind
                                        (second minute hour date month year)
                                      (decode-universal-time value)
                                    (declare (ignore second minute hour))
                                    (format nil "~a-~2,'0d-~2,'0d" year month date)))
                                 (t value))
                        :type (cond
                                ((eq type 'date) "date")
                                ((subtypep type 'number) "number")
                                ((subtypep type 'boolean) "checkbox")
                                (t "text"))
                        :list (when (config-suggested-values page-config)
                                (format nil "~a-list" namestring))))

                ;; suggested values
                (when (config-suggested-values page-config)
                  (datalist (:id (format nil "~a-list" namestring))
                    (loop :for suggested :in (config-suggested-values page-config)
                          :collect (option (:value suggested)))))))))))


    (cond
      ((config-edit-ui-generator page-config)
       input-form)

      ((mobile-browser-p)
       (list
        (tr ()
          (td () (or (config-display-name page-config)
                     namestring)))
        (tr ()
          (td () input-form))
        (tr ()
          (td () (hr ())))))

      (t (tr ()
           (td () (or (config-display-name page-config)
                      namestring))
           (td () input-form))))))

(defparameter *foo* nil)

(defun derive-edit-page-from-table (table-name)
  (let ((def (find-table table-name)))

    (register-page
     (table-url def "edit")
     (lambda-with-parameters (id back-url)
       (let ((table-value (funcall (table-get-function def)            
                                   (parse-integer id :junk-allowed t))))
         (with-internal-page
           (hr ())
           (form (:method "post" :action (table-url def "save" (cons :id id))
                   :id (format nil "~a-form" (table-namestring def)))

             ;; Warn on unload if data has not been saved
             (span ()
               (format
                nil 
                "<script>
                let dirty = false;
                
                const form = document.querySelector(\"#~a\");
                
                form.addEventListener(\"input\", () => {
                    dirty = true;
                });
                
                form.addEventListener(\"submit\", () => {
                    dirty = false;
                });
                
                window.addEventListener(\"beforeunload\", (event) => {
                    if (dirty) {
                        event.preventDefault();
                        event.returnValue = \"\";
                    }
                });
                </script>" (format nil "~a-form" (table-namestring def))))
             
             (html-table ()
               (tr ()
                 (td ()
                   (button (:type "submit" :name "redirect-url"
                            :value (if back-url back-url (table-url def "list")))
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
             (html-table (:id "rows")

               ;; Create a edit row for table field
               (loop :for field :in (remove "id" (table-fields def)
                                            :key #'field-namestring
                                            :test #'string=)
                     :collect
                     (span (:id (format nil "~a-edit" (field-namestring field)))
                       (generate-form-input-from-field
                        :id (or (parse-integer id :junk-allowed t) 0)
                        :def def
                        :field field
                        :value (funcall (field-accessor field) table-value))))))
           
           ;; delete modal for deleting a record
           (dialog (:id "confirm-delete")
             (html-table ()
               (tr ()
                 (td ()
                   (a (:href (table-url
                              def "delete"
                              (cons :id (funcall (table-id-accessor def)
                                                 table-value))))
                     (button () "Permanently Delete?"))))
               (tr ()
                 (td ()
                   (hr ())))
               (tr ()
                 (td ()
                   (button (:command "close"
                            :commandfor "confirm-delete"
                            :type "button")
                     "Cancel")))))))))))

(defun derive-delete-page-from-table (table-name)
  (let ((def (find-table table-name)))
    (register-page
     (table-url def "delete")
     (lambda-with-parameters (id)
       (perform-auth-check)
       (let* ((val (funcall (table-get-function def)
                            (parse-integer id :junk-allowed t))))
         (funcall (table-delete-function def) val)
         (hunchentoot:redirect (table-url def "list")
                               :code 303))))))

(defun derive-view-reference-page-from-table (table-name)
  (let ((def (find-table table-name)))
    (register-page
     (table-url def "view-reference")
     (lambda-with-parameters (id field-name back-url)
       (perform-auth-check)
       (let* ((field (find field-name (table-fields def)
                           :key #'field-namestring :test #'string=))
              (foreign-def (ignore-errors
                            (find-table (field-references field)))))
         (if (null foreign-def)
             (h1 () "Error, could not find field " field-name)
             (let* ((table-value (funcall (table-get-function def)
                                          (parse-integer id :junk-allowed t)))
                    (foreign-id (funcall (field-accessor field) table-value)))
               (hunchentoot:redirect
                (table-url foreign-def "edit" (cons :id foreign-id)
                           (cons :back-url back-url))
                :code 303))))))))
