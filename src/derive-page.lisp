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
                                   :collect (string-downcase
                                             (format nil "~a=~~a" key))))
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
          `(pushnew (make-tab :name ,(format nil "[~a list]" (table-namestring def))
                              :url ,(generate-table-url def "list"))
                    *toplevel-tabs* :test #'equalp))

       (hunchentoot:define-easy-handler
           (,(open-orders.fn:symbolicate table-name '-list)
            :uri ,(generate-table-url def "list"))
           (sort-by reverse)
         (with-internal-page
           (hr ())
           (html-table ()
             (tr ()
               (td ()
                 (form (:action ,(format nil "/~a/new" (table-namestring def)))
                   (input (:type "submit"
                           :value ,(format nil "new ~a" (table-namestring def))))))))
           (hr ())
           (html-table ()

             ;; table header
             (tr ()
               ,@(mapcar
                  (lambda (field)
                    `(th ()
                       (a (:href
                           ,(generate-table-url
                             def "list"
                             :sort-by (field-namestring field)
                             :reverse '(if (string= reverse "true") "false" "true")))
                         ',(field-name field))))
                  listed-fields))

             ;; table body
             (loop
               :for val
                 :across
                 (let* ((sorted (sort (open-orders.database::table-get-all
                                       *database-path* ',table-name)
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
                 ,@(mapcar (lambda (field)
                             `(td ()
                                (a (:href ,(generate-table-url
                                            def
                                            "edit"
                                            :id `(,(table-id-accessor def) val)))
                                  (,(field-accessor field) val))))
                           listed-fields)))))))))

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

(defmacro derive-save-page-from-table (table-name)
  (let ((def (find-table table-name)))
    `(hunchentoot:define-easy-handler
         (,(open-orders.fn:symbolicate table-name '-save)
          :uri ,(format nil "/~a/save" (table-namestring def)))
         (id)
       (let* ((params (hunchentoot:post-parameters*))
              (id (parse-integer id))
              (redirect-url (geta "redirect-url" params))
              (val (,(table-get-function def) id)))

         ;; iterate over all fields, getting their values from
         ;; parameters and setting them when non-null
         ,@(mapcar (lambda (field)
                     `(let ((field-value (geta ,(field-namestring field) params)))
                        (when field-value
                          (setf (,(field-accessor field) val)
                                field-value)))
                     )
                   (table-fields def))
         (,(table-set-function def) val)

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

(defmacro derive-edit-page-from-table (table-name)
  (let ((def (find-table table-name))) 
    `(define-edit-page-for-table ,table-name () (save-url table-value)
       (with-internal-page
         (hr ())
         (form (:method "post" :action save-url)
           (html-table ()
             (tr ()
               (td ()
                 (button (:type "submit" :name "redirect-url"
                          :value ,(table-url def "list"))
                   "back"))
               (td ()
                 (button (:type "submit" :name "redirect-url"
                          :value (hunchentoot:request-uri*))
                   "save"))))
           (hr ())
           (html-table ()
             ,@(mapcar
                (lambda (field)
                  `(tr ()
                     (td () ,(field-namestring field))
                     (td ()
                       (input (:name ,(field-namestring field)
                               :value (,(field-accessor field) table-value))))))
                (table-fields def))))))))

