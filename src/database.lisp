(uiop:define-package #:open-orders.database
  (:use #:cl)
  (:import-from #:defclass-std
                #:defclass/std)
  (:export #:database
           #:dbget
           #:new-id
           #:insert
           #:serialize
           #:deserialize))
(in-package open-orders.database)

(defmethod marshal:class-persistent-slots ((serializable standard-object))
  (closer-mop:ensure-finalized (class-of serializable))
  (mapcar #'closer-mop:slot-definition-name
          (closer-mop:class-slots (class-of serializable))))














(defclass/std database ()
  ((id-counter :std 0 :type integer)
   (elements :std (make-hash-table))))

(defun dbget (database id)
  (gethash id (elements database)))
(defun (setf dbget) (value database id)
  (setf (gethash id (elements database)) value))
(defun new-id (database)
  (let ((id (id-counter database)))
    (incf (id-counter database))
    id))
(defun insert (database item)
  (let ((id (new-id database)))
    (setf (dbget database id) item)
    id))


(cffi:define-foreign-library libc
  (:default "libc"))
(cffi:use-foreign-library libc)

(cffi:defcfun fileno :int (stream :pointer))
(cffi:defcfun fopen :pointer (path :string) (modes :string))
(cffi:defcfun fputc :int (ch :int) (stream :pointer))
(cffi:defcfun fclose :int (stream :pointer))
(cffi:defcfun "strerror" :string (errnum :int))
(cffi:defcvar ("errno" *errno*) :int)

#-windows
(progn
  (cffi:defcfun fsync :int (fd :int))
  (cffi:defcfun ("rename" frename) :int (oldpath :string) (newpath :string)))

;;; TODO actually test this windows code to make sure it works
#+windows
(progn
  (cffi:defcfun ("_commit" fsync) :int (fd :int))
  (cffi:defcfun "ReplaceFileA" :int
    (replaced-file-name :string)
    (replacement-file-name :string)
    (backup-file-name :string)
    (replace-flags :int)
    (lp-exclude :pointer)
    (lp-reserved :pointer))
  (defun frename (oldpath newpath)
    (let ((err (replace-file-a oldpath newpath
                               (cffi:null-pointer)
                               0
                               (cffi:null-pointer)
                               (cffi:null-pointer)
                               )))
      (when (not (zerop err))
        (error "replace file failed: ~a" err)))))

(defun safe-overwrite-file (filepath string)
  "Overwrite file fully or not at all (atomically),
   in order to prevent corruption during a crash."
  (let* ((tmppath (concatenate 'string filepath ".temp"))
         (fp (fopen tmppath "wb")))
    (when (cffi:pointer-eq fp (cffi:null-pointer))
      (error "fopen failed: ~a" (strerror *errno*)))
    (loop :for ch :across string
          :do (fputc (char-code ch) fp))
    (unless (zerop (fsync (fileno fp)))
      (error "fsync failed: ~a" (strerror *errno*)))
    (unless (zerop (frename tmppath filepath))
      (error "rename failed: ~a" (strerror *errno*)))
    (fclose fp)))

(defun serialize (database filepath)
  (safe-overwrite-file
   filepath
   (format nil "~S" (marshal:marshal database))))

(defun deserialize (filepath)
  (marshal:unmarshal
   (uiop:read-file-form filepath)))

#+nil
(defun test ()
  (let ((db (make-instance 'database)))
    (insert db "foo")
    (insert db "bar")
    (insert db "as;ldkfj;asldkjf")
    (serialize db "campro.database")
    ))



