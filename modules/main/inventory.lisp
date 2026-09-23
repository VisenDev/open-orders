(defpackage #:open-orders.main.inventory
  (:use #:cl #:open-orders.pagen #:open-orders.tables))
(in-package #:open-orders.main.inventory)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *tables* (make-hash-table :test 'equal))
  (defvar *file-extension* "sexp")
  (defvar *database-path* "database/"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct param
    (name nil :type symbol)
    (type t)
    (kind nil :type (member :required :optional :key :rest :aux))
    initform)
  
  (defun parse-typed-lambda-list (params)
    (let ((state :required)
          (results nil))
      (loop :for val :in params
            :do (case val
                  (&optional (setf state :optional))
                  (&key (setf state :key))
                  (&rest (setf state :rest))
                  (&aux (setf state :aux))
                  (otherwise
                   (case state
                     (:required
                      (cond ((listp val)
                             (assert (= 2 (length val)))
                             (push (make-param :name (first val)
                                               :kind :required
                                               :type (second val))
                                   results))
                            (t (push (make-param :name val
                                                 :kind :required)
                                     results))))
                     ((:optional :key :aux)
                      (cond ((and (listp val) (listp (car val)))
                             (assert (= 2 (length (car val))))
                             (push (make-param :name (first (first val))
                                               :kind state
                                               :type (second (first val))
                                               :initform (second val))
                                   results))
                            ((listp val)
                             (push (make-param :name (first val)
                                               :kind state
                                               :initform (second val))
                                   results))
                            (t (push (make-param :name val
                                                 :kind state)
                                     results))))

                     (:rest
                      (push (make-param :name val
                                        :kind :rest)
                            results))))))
      (nreverse results)))

  (defun generate-function-type (return-type parsed-typed-lambda-list)
    (loop
      :with results = nil
      :with active-kind = :required
      :for param :in parsed-typed-lambda-list
      :for name = (param-name param)
      :for type = (param-type param)
      :for kind = (param-kind param)
      :do (progn
            (when (not (eq kind active-kind))
              (case kind
                (:optional (push '&optional results))
                (:key (push '&key results))
                (:rest (push '&rest results)))

              (setf active-kind kind))
            (ecase kind
              ((:required :optional :rest) (push type results))
              (:key (push (list (intern (symbol-name name) 'keyword) type)
                          results))
              (:aux))
            results)
      :finally
         (return `(function ,(nreverse results) ,return-type))))
  
  (defun generate-function-lambda-list (parsed-typed-lambda-list)
    (loop
      :with results = nil
      :with active-kind = :required
      :for param :in parsed-typed-lambda-list
      :for name = (param-name param)
      :for kind = (param-kind param)
      :for initform = (param-initform param)
      :do (progn
            (when (not (eq kind active-kind))
              (push (ecase kind
                      (:optional '&optional)
                      (:key '&key)
                      (:rest '&rest)
                      (:aux '&aux))
                    results)
              (setf active-kind kind))
            (push 
             (ecase kind
               ((:required :rest) name)
               ((:optional :aux :key) (list name initform)))
             results))
      :finally
         (return (nreverse results)))))

(defmacro fn (name typed-lambda-list &body body)
  "name -> either a function name or a list of the form (function-name return-type),
   typed-lambda-list -> a typed parameter list like a defmethod parameter list
   body -> a normal function body"
  (let* ((function-name (if (listp name) (first name) name))
         (return-type (if (listp name) (second name) t))
         (params (parse-typed-lambda-list typed-lambda-list)))
    `(progn (declaim (ftype ,(generate-function-type return-type params)
                            ,function-name))
            (defun ,function-name ,(generate-function-lambda-list params)
              (declare ,@(mapcar (lambda (param)
                                   `(type ,(param-type param)
                                          ,(param-name param)))
                                 params))
              (the ,return-type (progn ,@body))))))




#+clisp
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "syscalls"))

#+(and sbcl (not win32))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-posix))

;; Code ported from uiop, because rename can't replace a file on ecl
(fn (rename-file-overwriting-target t) ((source pathname)
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
(fn (valid-table-designator-char-p boolean) ((ch character))
  (or (alphanumericp ch) (char= #\_ ch) (char= #\- ch)))

(fn (valid-table-designator-p boolean) ((symbol symbol))
  (every #'valid-table-designator-char-p (symbol-name symbol)))

(deftype table-designator () 
  `(and symbol (satisfies valid-table-designator-p)))

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
             (rename-file-overwriting-target tmp-filename id-filename)

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
          (format fp "~S" table-value))
      (file-error (e)
        (if (> retries 3)
            (error e)
            ;; else retry
            (progn
              (return-from table-set
                (table-set database-path table-value id (1+ retries)))))))

    ;; rename tmp file on success
    (handler-case 
        (progn (rename-file-overwriting-target tmp-pathname output-pathname)
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
                  (name &key type compare-function initform references)) )
    (name nil :type symbol)
    accessor
    (type t)
    compare-function
    initform
    references)
  (defstruct table
    (name nil :type table-designator)
    id-accessor
    (fields nil :type list)
    (conc-name nil :type symbol)))

;;; For slime completion of arguments
;;; Not actually used for anything
;; (defmacro field (name &key type compare-function initform references)
;;   (declare (ignore name type compare-function initform references)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun symbolicate (&rest things)
    (intern (string-upcase
             (apply #'concatenate 'string
                    (mapcar (lambda (thing) (format nil "~a" thing)) things)))))
  (defun parse-table-definition (name field-forms conc-name)
    (let ((conc-name (or conc-name (symbolicate name '-))))
      (make-table :name name
                  :conc-name conc-name
                  :id-accessor (symbolicate conc-name 'id)
                  :fields (cons
                           (let ((id (field 'id :type '(or null integer))))
                             (setf (field-accessor id)
                                   (symbolicate conc-name 'id))
                             id)
                           (mapcar
                            (lambda (field-form)
                              (assert (eq 'field (first field-form)))
                              (let ((field (apply #'field (cdr field-form))))
                                (unless (field-accessor field)
                                  (setf (field-accessor field)
                                        (symbolicate conc-name (field-name field))))
                                field))
                            field-forms))))))


(defmacro define-table (name fields &key conc-name)
  (let ((def (parse-table-definition name fields conc-name)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (gethash ,(symbol-name name) *tables*)
               (parse-table-definition
                ',name
                ',fields
                ',conc-name)))
       (defstruct (,(table-name def) (:conc-name ,(table-conc-name def)))
         ,@(mapcar (lambda (f)
                     (list (field-name f) (field-initform f)
                           :type (field-type f)))
            (table-fields def)))

       ,(macroexpand
         `(fn (,(symbolicate 'get- (table-name def)) (or null ,(table-name def)))
              ((id integer)
               &optional ((database-path (or string pathname)) *database-path*))
            (table-get database-path ',(table-name def) id)))

       ,(macroexpand
         `(fn (,(symbolicate 'set- (table-name def)) t)
              ((,(table-name def) ,(table-name def))
               &optional ((database-path (or string pathname)) *database-path*))
            (let ((id (,(table-id-accessor def) ,(table-name def))))
              (declare (type (or integer null) id))
              (unless id
                (setf id (table-find-free-id database-path ',(table-name def)))
                (setf (,(table-id-accessor def) ,(table-name def)) id))
              (table-set database-path ,(table-name def) id))))

       ,(macroexpand
         `(fn (,(symbolicate 'get-every- (table-name def))
               (vector ,(table-name def) *))
              (&optional ((database-path (or string pathname)) *database-path*))
            (table-get-all database-path ',(table-name def)))))))

(defmacro with-database (path &body body)
  `(let ((*database-path* ,path))
     ,@body))



;;;; TESTS
#+nil
(progn
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
        (* thread-count persons-per-thread)))))
