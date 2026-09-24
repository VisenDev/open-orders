(defpackage #:open-orders.database
  (:use #:cl #:open-orders.fn)
  (:export

   ;; For defining new tables
   #:define-table
   #:field

   ;; File extension of database files, defaults to .sexp
   #:*file-extension*

   ;; Path to the database on the file system
   #:*database-path*

   ;; Extension mechanism for defining custom functions to perform atomic replace
   #:*atomic-replace-file-function*

   ;; Hashtable of keyed by symbol-name of all tables
   #:*tables*

   ;; Field structure
   #:field-p
   #:copy-field
   #:field-name
   #:field-accessor
   #:field-type
   #:field-compare-function
   #:field-initform
   #:field-references
   #:field-metadata
   #:field-docs
   #:field-namestring

   ;; Table structure
   #:table
   #:make-table
   #:table-p
   #:copy-table
   #:table-name
   #:table-id-accessor
   #:table-fields
   #:table-conc-name
   #:table-namestring
   #:table-get-every-function
   #:table-get-function
   #:table-set-function
   #:table-constructor))
(in-package #:open-orders.database)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *tables* (make-hash-table :test 'equal)))
(defvar *file-extension* "sexp")
(defvar *database-path* "database/")
(defvar *atomic-replace-file-function* 'default-atomic-replace-file
  "A function designator that takes two arguments, source and target. 
   The 'source' file should be atomically renamed to 'target', overwriting
   'target' if it already exists.")


#+clisp
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "syscalls"))

#+(and sbcl (not win32))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-posix))

;; Default function that can atomically replace files,
;; might not work on all implementations/platforms
;; so custom atomic replace functions can be used
;; by setting the *atomic-replace-file-function* variable
(fn (default-atomic-replace-file t) ((source pathname)
                                     (target pathname))
  #+(and sbcl (not win32))
  (sb-posix:rename source target)

  #+clisp
  (posix:copy-file source target :method :rename)

  #+(and sbcl win32)
  (progn
    ;; Not atomic.
    (handler-case
        (delete-file target)
      (file-error () nil))
    (rename-file source target))

  #-(or clisp sbcl)
  (rename-file source target
               #+(or clasp clozure ecl) :if-exists
               #+clozure :rename-and-delete
               #+(or clasp ecl) t))

;;;; DATABASE IMPLEMENTATION
(eval-when (:compile-toplevel :load-toplevel :execute)
  (fn (valid-table-designator-char-p boolean) ((ch character))
    (or (alphanumericp ch) (char= #\_ ch) (char= #\- ch)))

  (fn (valid-table-designator-p boolean) ((symbol symbol))
    (every #'valid-table-designator-char-p (symbol-name symbol)))

  (deftype table-designator () 
    `(and symbol (satisfies valid-table-designator-p))))

(fn (table-directory-get pathname)
    ((database-path (or string pathname))
     (table-name table-designator))
  (unless (string= (directory-namestring database-path)
                   (namestring database-path))
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
    (declare (dynamic-extent filename))
    (when (probe-file filename)
      (with-open-file (fp filename)
        (let ((cl:*read-eval* nil))
          (unless (zerop (file-length fp))
            (read fp)))))))

;; Lock file creation for syncronizing threads
(fn (try-acquire-lock (or null stream)) ((pathname pathname))
  (open pathname
        :direction :output
        :if-exists nil
        :if-does-not-exist :create))

(fn (acquire-lock t) ((pathname pathname))
  (loop
    :for stream = (try-acquire-lock pathname)
    :when stream
      :do (close stream)
         (return)
    :do (sleep 0.001)))

(fn (release-lock t) ((pathname pathname))
  (delete-file pathname))

;; Finds free id using a NEXT-ID file
;; Protected by an ID-LOCK file
(fn (table-find-free-id integer) ((database-path (or pathname string))
                                  (table-name table-designator))


  (let* ((dir (table-directory-get database-path table-name))
         (lock-filename (merge-pathnames "ID-LOCK" dir))
         (id-filename (merge-pathnames "NEXT-ID" dir)))
    (declare (dynamic-extent dir lock-filename id-filename))
    
    (acquire-lock lock-filename)
    (unwind-protect
         (progn
           (unless (probe-file id-filename)
             
             ;; regenerate id file
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
               (with-open-file (fp id-filename :if-does-not-exist :create
                                               :direction :output)
                 (format fp "~S" new-id))))

           ;; read id
           (let ((id (with-open-file (fp id-filename)
                       (let ((cl:*read-eval* nil))
                         (read fp))))
                 (tmp-filename (merge-pathnames "NEXT-ID.tmp" dir)))
             (declare (dynamic-extent tmp-filename))
             (assert (integerp id))

             ;; write new id to tmp file
             (with-open-file (fp tmp-filename
                                 :direction :output
                                 :if-exists :supersede
                                 :if-does-not-exist :create)
               (format fp "~S" (1+ id)))

             ;; overwrite id file with tmp file
             (funcall *atomic-replace-file-function*
                      tmp-filename id-filename)
             id)
           
           )
      (release-lock lock-filename))))

(fn (table-set integer) ((database-path (or string pathname))
                         (table-value t)
                         (id integer)
                         &optional
                         ((retries integer) 0))
  (let* ((table-name (class-name (class-of table-value)))
         (directory (truename (table-directory-get database-path table-name)))
         (tmp-pathname (merge-pathnames
                        (format nil "~a-~a.tmp" (random 10000000) id)
                        directory))
         (output-pathname (merge-pathnames
                           (format nil "~a.~a" id *file-extension*)
                           directory)))
    (declare (dynamic-extent table-name directory tmp-pathname output-pathname))
    (handler-case
        (with-open-file (fp tmp-pathname :direction :output :if-exists :error)
          (let ((*package* (find-package 'cl)))
            (format fp "~S" table-value)))
      (file-error (e)
        (if (> retries 3)
            (error e)
            ;; else retry
            (progn
              (return-from table-set
                (table-set database-path table-value id (1+ retries)))))))

    ;; rename tmp file on success
    (handler-case 
        (progn (funcall *atomic-replace-file-function*
                        tmp-pathname output-pathname)
               ;; return id
               id)
      (file-error (e)

        ;; clean up tempfile if rename fails and output pathname still exists
        (when (and (probe-file tmp-pathname) (probe-file output-pathname))
          (ignore-errors (delete-file tmp-pathname)))
        (error e)))))

(fn (table-get-all vector) ((database-path (or string pathname))
                            (table-name table-designator)
                            &optional
                            ((output-buffer vector)
                             (make-array 7 :adjustable t :fill-pointer 0)))
  (assert (adjustable-array-p output-buffer))
  (assert (array-has-fill-pointer-p output-buffer))
  (setf (fill-pointer output-buffer) 0)
  (loop :for path :in (directory
                       (merge-pathnames
                        (make-pathname :name :wild :type *file-extension*)
                        (table-directory-get database-path table-name)))
        :do
           (with-open-file (fp path)
             (unless (zerop (file-length fp))
               (let ((cl:*read-eval* nil))
                 (vector-push-extend (read fp) output-buffer))))
        :finally (return output-buffer)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct (field
              (:constructor field
                  (name &key type compare-function initform
                          references metadata docs)))
    (name nil :type symbol)
    (namestring "" :type string)
    accessor
    (type t)
    compare-function
    initform
    references
    metadata
    docs)
  (defstruct table
    (name nil :type table-designator)
    (namestring "" :type string)
    constructor
    id-accessor
    get-every-function
    get-function
    set-function
    (fields nil :type list)
    (conc-name nil :type symbol)))

;;; For slime completion of arguments
;;; Not actually used for anything
;; (defmacro field (name &key type compare-function initform references)
;;   (declare (ignore name type compare-function initform references)))

(eval-when (:compile-toplevel :load-toplevel :execute)
    (defun parse-table-definition (name field-forms conc-name id-field-metadata)
    (let ((conc-name (or conc-name (symbolicate name '-))))
      (make-table :name name
                  :conc-name conc-name
                  :id-accessor (symbolicate conc-name 'id)
                  :get-every-function (symbolicate 'get-every- name)
                  :get-function (symbolicate 'get- name)
                  :set-function (symbolicate 'set- name)
                  :namestring (string-downcase (symbol-name name))
                  :constructor (symbolicate 'make- name)
                  :fields (cons
                           (let ((id (field 'id :type '(or null integer)
                                                :metadata id-field-metadata)))
                             (setf (field-namestring id) "id")
                             (setf (field-accessor id)
                                   (symbolicate conc-name 'id))
                             id)
                           (mapcan
                            (lambda (field-form)
                              (assert (eq 'field (first field-form)))
                              
                              (let ((name-or-names (second field-form))
                                    (clauses (cddr field-form))
                                    (forms nil))

                                ;; check for list of names
                                ;; creating a copy of clauses for each name
                                (if (listp name-or-names)
                                    (mapcar
                                     (lambda (name)
                                       (push (cons 'field (cons name clauses))
                                             forms))
                                     name-or-names)
                                    (push field-form forms))

                                ;; map all forms and derived forms
                                (mapcan
                                 (lambda (form)
                                   (let ((field (apply #'field (cdr form))))
                                     (unless (field-accessor field)
                                       (setf (field-accessor field)
                                             (symbolicate conc-name
                                                          (field-name field)))
                                       (setf (field-namestring field)
                                             (string-downcase
                                              (symbol-name (field-name field)))))
                                     (list field)))
                                 forms)))
                            field-forms))))))


(defmacro define-table (name fields &key conc-name id-field-metadata)
  "'fields' should be a list of s-expressions of the form
   (field <name> :type <type> ...etc...)

   The field declaration at the beginning is so that slime
   will help you know what keyword arguments are allowed

   Also note that multiple names may be specified if <name> is
   a list of names rather than just one name, in which case
   many similar fields can be defined quickly, ie,

   (define-table vec2 
     ((field (x y) :type float :initform 0.0f0)))

   ;; is equivalent to

   (define-table vec2 
     ((field x :type float :initform 0.0f0)
      (field y :type float :initform 0.0f0)))

   For a complete list of field options, look at the definition of the 'field'
   struct. "
  (let ((def (parse-table-definition name fields conc-name id-field-metadata)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (gethash ,(symbol-name name) *tables*)
               (parse-table-definition
                ',name
                ',fields
                ',conc-name
                ',id-field-metadata)))
       (defstruct (,(table-name def) (:conc-name ,(table-conc-name def)))
         ,@(mapcar (lambda (f)
                     (list (field-name f) (field-initform f)
                           :type (field-type f)))
            (table-fields def)))

       ,(macroexpand
         `(fn (,(table-get-function def) (or null ,(table-name def)))
              ((id integer)
               &optional ((database-path (or string pathname)) *database-path*))
            (table-get database-path ',(table-name def) id)))

       ,(macroexpand
         `(fn (,(table-set-function def) t)
              ((,(table-name def) ,(table-name def))
               &optional ((database-path (or string pathname)) *database-path*))
            (let ((id (,(table-id-accessor def) ,(table-name def))))
              (declare (type (or integer null) id))
              (unless id
                (setf id (table-find-free-id database-path ',(table-name def)))
                (setf (,(table-id-accessor def) ,(table-name def)) id))
              (table-set database-path ,(table-name def) id))))

       ,(macroexpand
         `(fn (,(table-get-every-function def)
               (vector ,(table-name def) *))
              (&optional ((database-path (or string pathname)) *database-path*))
            (table-get-all database-path ',(table-name def)))))))

(defmacro with-database (path &body body)
  `(let ((*database-path* ,path))
     ,@body))



;;;; TESTS
(define-table person
    ((field (first-name last-name email phone) :type string :initform "")
     (field age :type integer :initform 0)
     (field notes :type list))
  :conc-name p-)

(define-table customer
    ((field (name description) :type string :initform "")
     (field contact :references person))
  :conc-name c-)

;; Concurrent insert brute force test
#+nil
(defun test-concurrent-person-inserts (&key (thread-count 10)
                                         (persons-per-thread 1000))
  (with-database "database/"
    (let ((threads
            (loop for thread-id below thread-count
                  collect
                  (bt:make-thread
                   (lambda ()
                     (loop for i below persons-per-thread
                           do (set-person
                               (make-person
                                :first-name (format nil "First-~D-~D" thread-id i)
                                :last-name  (format nil "Last-~D-~D" thread-id i)
                                :email      (format nil "person-~D-~D@example.com"
                                                    thread-id i)
                                :phone      (format nil "~D-~D" thread-id i)))))
                   :name (format nil "person-writer-~D" thread-id)))))
      (mapc #'bt:join-thread threads)
      (* thread-count persons-per-thread))))
