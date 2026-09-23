(defpackage #:open-orders.main.inventory
  (:use #:cl #:open-orders.pagen #:open-orders.tables))
(in-package #:open-orders.main.inventory)

(defvar *tables* (make-hash-table :test 'equal))
(defvar *file-extension* "sexp")

(defmacro fn (name params &body body)
  "name -> either a function name or a list of the form (function-name return-type),
   params -> a typed parameter list equivalent to a defmethod parameter list
   body -> a normal function body"
  (let* ((function-name (if (listp name) (first name) name))
         (return-type (if (listp name) (second name) t))
         (normalized-params (mapcar (lambda (param)
                                      (if (listp param)
                                          param
                                          (list param t)))
                                    params)))
    `(progn (declaim (ftype (function
                             ,(mapcar #'second normalized-params)
                             ,return-type)
                            ,function-name))
            (defun ,function-name ,(mapcar #'first normalized-params)
              (declare ,@(mapcar (lambda (param) `
                                   (type ,(second param) ,(first param)))
                                 normalized-params))
              (the ,return-type (progn ,@body))))))

(fn (valid-table-designator-char-p boolean) ((ch character))
  (or (alphanumericp ch) (char= #\_ ch) (char= #\- ch)))

(fn (valid-table-designator-p boolean) ((symbol symbol))
  (every #'valid-table-designator-char-p (symbol-name symbol)))

(deftype table-designator () 
  `(and symbol (satisfies valid-table-designator-p)))

(fn (table-directory-get pathname) ((database-path (or string pathname))
                                   (table-name table-designator))
  (unless (string= (directory-namestring database-path)
                        database-path)
    (error "database-path must end with a /"))
  (ensure-directories-exist
   (merge-pathnames (string-downcase
                     (concatenate 'string (symbol-name table-name) "/"))
                    database-path)))

(fn (table-filename-get pathname) ((database-path (or string pathname))
                               (table-name table-designator)
                               (id integer))
  (merge-pathnames
   (format nil "~a.~a" id *file-extension*)
   (table-directory-get database-path table-name)))

(fn (table-get t) ((database-path (or string pathname))
                                            (table-name table-designator)
                                            (id integer))
  (let ((filename (table-filename-get database-path table-name id)))
    (when (probe-file filename)
      (with-open-file (fp filename)
        (let ((cl:*read-eval* nil))
          (read fp))))))

(defun find-free-id (database-path table-name &optional (depth 0))

  (let* ((paths (directory
                 (merge-pathnames
                  (make-pathname :name :wild :type *file-extension*)
                  (table-directory-get database-path table-name))))
         (names (mapcar #'pathname-name paths))
         (cl:*read-eval* nil)
         (nums (mapcar #'read-from-string names))
         (new-id
           (if (endp nums)
               0
               (1+ (reduce #'max nums)))))

    ;; Write file to reserve id from other threads
    (handler-case
        (with-open-file
            (fp (table-filename-get database-path table-name new-id)
                :direction :output
                :if-exists :error
                :if-does-not-exist :create)
          new-id)
      (file-error (err)
        (if (< depth 10)
            (find-free-id database-path table-name (1+ depth))
            (error err))))))

(fn (table-set t) ((database-path (or string pathname))
                   (table-value t)
                   (id integer))
  (let* ((table-name (class-name (class-of table-value)))
         (directory (truename (table-directory-get database-path table-name)))
         (tmp-pathname (merge-pathnames
                        (format nil "temp-~a.~a" (random 100000) *file-extension*)
                        directory))
         (output-pathname (merge-pathnames
                           (format nil "~a.~a" id *file-extension*)
                           directory)))
    (with-open-file (fp tmp-pathname :direction :output :if-exists :error)
      (format fp "~S" table-value))
    (rename-file tmp-pathname output-pathname)))

(fn (table-get-all list) ((database-path (or string pathname))
                          (table-name table-designator))
  (loop :for path :in (directory
                       (merge-pathnames
                        (make-pathname :name :wild :type *file-extension*)
                        (table-directory-get database-path table-name)))
        :collect (with-open-file (fp path)
                   (let ((cl:*read-eval* nil))
                     (read fp)))))

(defstruct field
  (name nil :type symbol)
  accessor
  type
  compare-function
  initform
  references)
(defstruct table
  (name nil :type table-designator)
  id-accessor
  (fields nil :type list)
  (conc-name nil :type symbol))

;;; For slime completion of arguments
;;; Not actually used for anything
(defmacro field (name &key type compare-function initform references)
  (declare (ignore name type compare-function initform references)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun symbolicate (&rest things)
    (intern (string-upcase
             (apply #'concatenate 'string
                    (mapcar (lambda (thing) (format nil "~a" thing)) things)))))
  (defun parse-table-definition (name fields conc-name)
    (let ((conc-name (or conc-name (symbolicate name '-))))
      (make-table :name name
                  :conc-name conc-name
                  :id-accessor (symbolicate conc-name 'id)
                  :fields (cons
                           (make-field :name 'id
                                       :accessor (symbolicate conc-name 'id)
                                       :type '(or null integer))
                           (mapcar
                            (lambda (field)
                              (destructuring-bind
                                  (_ field-name &key type compare-function
                                            initform references)
                                  field
                                (declare (ignore _))

                                (make-field
                                 :name field-name
                                 :type (or type t)
                                 :accessor (symbolicate conc-name field-name)
                                 :initform initform
                                 :compare-function compare-function
                                 :references references)))
                            fields))))))

(defvar *database-path* "")
(defmacro define-table (name fields &key conc-name)
  (let ((def (parse-table-definition name fields conc-name)))
    (setf (gethash (symbol-name name) *tables*) def)
    `(progn
       (defstruct (,(table-name def) (:conc-name ,(table-conc-name def)))
         ,@(mapcar (lambda (f)
                     (list (field-name f) (field-initform f)
                           :type (field-type f)))
            (table-fields def)))
       
       (defun ,(symbolicate 'get- (table-name def))
           (id
            &optional (database-path *database-path*))
         (table-get database-path ',(table-name def) id))
       
       (defun ,(symbolicate 'set- (table-name def))
           (,(table-name def)
            &optional (database-path *database-path*))
         (let ((id (,(table-id-accessor def) ,(table-name def))))
           (unless id
             (setf id (table-find-free-id database-path ',(table-name def)))
             (setf (,(table-id-accessor def) ,(table-name def)) id))
           (table-set database-path ,(table-name def) id)))

       (defun ,(symbolicate 'get-every- (table-name def))
           (&optional (database-path *database-path*))
         (table-get-all database-path ',(table-name def))))))

  
(define-table person
    ((field first-name :type string :initform "")
     (field last-name :type string :initform "")
     (field email :type string :initform "")
     (field phone :type string :initform ""))
  :conc-name p-)

(define-table customer
  ((field name :type string :initform "")
   (field contact :references person))
  :conc-name c-)


;; valid types
;; (defmacro define-table (name (&body fields)
;;                         (&key generate-pages-p))
;;   `(progn
;;      (defstruct ,name
;;        ,@(mapcar (lambda (field)
;;                    (if (listp field) `(,(first field) nil :type ,(second field))
;;                        field))
;;           fields))
;;      ,(when generate-pages-p
;;         `(hunchentoot:define-easy-handler (,name :uri ,(format nil "/~a/list" name))
;;             ()
;;           (perform-auth)
;;            (let ((items (table-get-all 'name)))
;;              (doctype ()
;;                (head ())
;;                (body ()
;;                  (table ()
;;                    (tr ()
;;                      ,(mapcar (lambda (field)
;;                                 `(th () ,(if (listp field) (first field) field))
;;                                )
;;                               fields))
;;                    (mapcar (lambda (item)
;;                              (tr ()
;;                                ()
;;                                )
;;                              )
;;                            items)))))
;;           )))
;;   )

;; (define-table person
;;     ((first-name string)
;;       (last-name string)
;;       (email string)
;;       (phone string))
;;     (:generate-pages-p t)
;;     )

;; (define-table customer
;;     ((name integer)
;;      (primary-contact person))
;;     )
